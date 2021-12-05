#![feature(ptr_as_uninit)]
use core::pin::Pin;
use core::mem::MaybeUninit;

/// A type that can be constructed in-place.
///
/// This trait is unsafe to implement due to the constraints on `emplace`.
pub unsafe trait Emplace<Args>: Sized {
    /// Initializes `place` with a valid value of `Self` using the given arguments.
    ///
    /// ## Safety (implementer)
    ///
    /// The implementor *must* initialize `place`. Callers are allowed to assume that `place` is
    /// initialized after this returns.
    ///
    /// ## Safety (caller)
    ///
    /// This function is safe to call only when `place` is uninitialized.
    /// Otherwise, it could be used to move out of a pinned value.
    //
    //  FIXME: Perhaps this should return a "witness of the initializedness of `place`" (e.g.
    //  `Pin<&mut Self`)? If callers were forced to check that the address of the returned pointer
    //  was equal to that of the input pointer, I think the trait itself would be safe to implement.
    unsafe fn emplace(place: Pin<&mut MaybeUninit<Self>>, args: Args);
}

/// Initializes a struct, field by field, in-place on the heap.
///
/// ## Examples
///
/// A translated version of the code from the [Rust-for-Linux talk at CTCFT][CTCFT] (Nov 2021).
///
/// [CTCFT]: https://youtu.be/azcrUzeY3Pw?t=680
///
/// ```ignore
/// struct SharedState {
///     state_changed: CondVar,
///     inner: Mutex<usize>,
/// }
///
/// fn try_new() -> Result<Ref<Self>> {
///     let ret = emplace!(UniqueRef <- SharedState {
///         state_changed <- CondVar("SharedState::state_changed"),
///         inner <- Mutex(0, "SharedState::inner"),
///     })?;
///
///     Ok(ret.into())
/// }
/// ```
///
/// This assumes [`Emplace`] is implemented for `Mutex` and `CondVar` and would require a small
/// tweak to the macro to support fallible allocation (`try_emplace`?).
#[macro_export]
macro_rules! emplace {
    ($P:ident <- $S:ident { $($fields:tt)* }) => {{
        use core::pin::Pin;
        use core::mem::{self, MaybeUninit};
        #[allow(unused)]
        use core::ptr::{self, addr_of_mut};

        let mut ret = $P::pin(MaybeUninit::uninit());
        let mut pret: Pin<&mut MaybeUninit<$S>> = ret.as_mut();

        emplace!(@INIT [pret] $($fields)*);

        // SAFETY: At this point, all fields that were given to the macro have been initialized.
        // The exhaustive pattern match below asserts that these are *all* fields of `S`, so `S` is
        // fully initialized. Therefore, it is safe to cast away the `MaybeUninit` (which is
        // `#[repr(transparent)]`).
        let ret: Pin<$P<$S>>  = unsafe { mem::transmute(ret) };
        let emplace!{ @PAT $S { $($fields)* }} = ret.as_ref().get_ref();

        ret
    }};

    // @INIT -- (Recursive) Initializes each field.

    (@INIT [$pret:ident]) => {};

    // Project a pinned mut ref to uninitialized `S` to a pinned mut ref of one of `S's
    // fields. Initialize it with `emplace`
    (@INIT [$pret:ident] $field:ident <- $F:ident ( $($args:expr),* $(,)? ), $($rest:tt)*) => {
        // SAFETY: $pret is uninitialized.
        unsafe {
            let pfield: Pin<&mut MaybeUninit<_>> = $pret
                .as_mut()
                .map_unchecked_mut(|p| {
                    let pfield = addr_of_mut!((*p.as_mut_ptr()).$field).as_uninit_mut();
                    pfield.unwrap() // cannot fail since `p` was non-null.
                });

            $F::emplace(pfield, ($($args),*));
        }

        emplace!(@INIT [$pret] $($rest)*);
    };

    (@INIT [$pret:ident] $field:ident : $expr:expr, $($rest:tt)*) => {
        unsafe {
            let pfield = addr_of_mut!((*$pret.as_mut().get_unchecked_mut().as_mut_ptr()).$field);
            ptr::write(pfield, $expr);
        }

        emplace!(@INIT [$pret] $($rest)*);
    };

    // @PAT -- Generates an exhaustive match for the struct we're constructing.
    //         This ensures all fields are initialized.

    (@PAT $S:ident { $( $field:ident $(:)? $(<-)? $_:expr, )* }) => {
        $S { $( $field : _, )* }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::cell::Cell;
    use core::marker::PhantomPinned;

    struct Mutex<T> {
        data: T,
        name: Cell<Option<&'static str>>,
        _pinned: PhantomPinned,
    }

    impl<T> Mutex<T> {
        unsafe fn new(x: T) -> Self {
            Mutex {
                data: x,
                name: Cell::new(None),
                _pinned: PhantomPinned,
            }
        }

        fn init(self: Pin<&mut Self>, s: &'static str) {
            self.into_ref().get_ref().name.set(Some(s))
        }

        fn name(&self) -> &'static str {
            self.name.get().unwrap()
        }
    }

    unsafe impl<T> Emplace<(T, &'static str)> for Mutex<T> {
        #[warn(unsafe_op_in_unsafe_fn)]
        unsafe fn emplace(place: Pin<&mut MaybeUninit<Self>>, args: (T, &'static str)) {
            // SAFETY:
            // - `Mutex::new`: `Mutex::init` is called below.
            // - `map_unchecked_mut`: `emplace` requires that `place` is uninitialized, so nothing is
            //   actually pinned.
            let this = unsafe {
                place.map_unchecked_mut(|p| p.write(Mutex::new(args.0)))
            };

            Mutex::init(this, args.1)
        }
    }

    struct SharedState {
        x: u32,
        y: u32,
        inner: Mutex<i32>,
        other: Mutex<i64>,
    }

    impl SharedState {
        fn new() -> Pin<Box<Self>> {
            emplace!(Box <- Self {
                other <- Mutex(0, "SharedState::other"),
                x: 1,
                inner <- Mutex(0, "SharedState::inner"),
                y: 3,
            })
        }
    }

    #[test]
    fn emplace() {
        let x = SharedState::new();
        let x = x.as_ref().get_ref();
        assert_eq!("SharedState::inner", x.inner.name());
        assert_eq!("SharedState::other", x.other.name());
        let _ = x.other.data;
    }
}

