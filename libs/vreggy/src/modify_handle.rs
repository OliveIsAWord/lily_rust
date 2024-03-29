use dbg_pls::DebugPls;
use std::ops::{Deref, DerefMut};

/// A handle to a mutable reference that remembers whether its inner data was
/// accessed and/or modified. The handle does not inspect this inner data, so
/// any accesses to the mutable references are considered a "modify" unless
/// explicitly overwritten by [`set_modified(false)`](Self::set_modified).
#[derive(Debug, DebugPls)]
pub struct ModifyHandleInner<'a, T: ?Sized> {
    reference: &'a mut T,
    did_modify: Option<bool>,
}

impl<'a, T: ?Sized> ModifyHandleInner<'a, T> {
    /// Constructs a [`ModifyHandleInner`] using the given mutable reference.
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let mut buffer = vec![72, 69, 76, 76, 79];
    /// let handle = ModifyHandleInner::new(&mut buffer);
    /// ```
    #[must_use]
    pub fn new(reference: &'a mut T) -> Self {
        Self {
            reference,
            did_modify: None,
        }
    }

    pub fn as_handle<'outer>(&'outer mut self) -> ModifyHandle<'outer, 'a, T> {
        ModifyHandle(self)
    }

    /// Returns an immutable reference to the inner data. This is not considered
    /// an "access".
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let mut buffer = vec![87, 79, 82, 76, 68, 33];
    /// let cloned = buffer.clone();
    /// let handle = ModifyHandleInner::new(&mut buffer);
    ///
    /// assert_eq!(handle.get(), &cloned);
    /// assert!(!handle.was_accessed());
    /// assert!(!handle.was_modified());
    /// ```
    #[must_use]
    pub fn get(&self) -> &T {
        &self.reference
    }

    /// Returns a mutable reference to the inner data. This is considered an
    /// "access". It is also considered a "modify" unless overridden
    /// by [`set_modified(false)`](Self::set_modified).
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let mut buffer = vec![4, 2, 15, 1];
    /// let mut handle = ModifyHandleInner::new(&mut buffer);
    /// handle.request_mut().sort();
    ///
    /// assert!(handle.was_accessed());
    /// assert!(handle.was_modified());
    /// assert_eq!(buffer, [1, 2, 4, 15]);
    /// ```
    #[must_use = "use method `set_modified(true)` if you only want to manually signal a modification"]
    pub fn request_mut(&mut self) -> &mut T {
        if self.did_modify.is_none() {
            self.did_modify = Some(true);
        }
        self.reference
    }

    /// Returns whether the inner data has been modified.
    /// If [`set_modified(false)`](Self::set_modified) was previously called,
    /// this method will return `false`.
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let val = &mut 42;
    /// let mut handle = ModifyHandleInner::new(val);
    /// *handle.request_mut() = 413;
    ///
    /// assert!(handle.was_modified());
    /// assert_eq!(*val, 413);
    /// ```
    #[must_use]
    pub fn was_modified(&self) -> bool {
        self.did_modify.unwrap_or(false)
    }

    /// Returns whether the inner data has been accessed
    /// (i.e. whether [`request_mut`](Self::request_mut) has been called). This
    /// cannot be overriden with [`set_modified`](Self::set_modified).
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let mut message = String::from("OWO");
    /// let mut handle = ModifyHandleInner::new(&mut message);
    /// handle.request_mut().make_ascii_lowercase();
    /// handle.set_modified(false);
    ///
    /// assert!(handle.was_accessed());
    /// assert!(!handle.was_modified());
    /// assert_eq!(&message, "owo");
    /// ```
    #[must_use]
    pub fn was_accessed(&self) -> bool {
        self.did_modify.is_some()
    }

    /// Returns:
    /// - `None` if the inner data has not been accessed.
    /// - `Some(false)` if the inner data was accessed but not modified because
    ///   [`set_modified(false)`](Self::set_modified) was called.
    /// - `Some(true)` if the inner data was accessed and modified.
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let mut interesting_value = &mut ();
    /// let mut handle = ModifyHandleInner::new(interesting_value);
    /// assert_eq!(handle.get_modify_state(), None);
    ///
    /// let _ = handle.request_mut();
    /// handle.set_modified(false);
    /// assert_eq!(handle.get_modify_state(), Some(false));
    ///
    /// handle.set_modified(true);
    /// assert_eq!(handle.get_modify_state(), Some(true));
    /// ```
    #[must_use]
    pub fn get_modify_state(&self) -> Option<bool> {
        self.did_modify
    }

    /// Overrides the previous remembered state of whether the inner data has
    /// been modified. This is considered an "access". If `false` is passed,
    /// then any previous or subsequent call to
    /// [`request_mut`](Self::request_mut) will no longer be considered a
    /// "modify".
    /// ```
    /// # use vreggy::ModifyHandleInner;
    /// let mut handle = {
    ///     let very_interesting_value = Box::new(());
    ///     ModifyHandleInner::new(Box::leak(very_interesting_value))
    /// };
    /// handle.set_modified(false);
    /// let _ = handle.request_mut();
    /// assert!(!handle.was_modified());
    ///
    /// handle.set_modified(true);
    /// assert!(handle.was_modified());
    /// ```
    pub fn set_modified(&mut self, modified: bool) -> Option<bool> {
        self.did_modify.replace(modified)
    }
}
/// A handle to a mutable reference that remembers whether its inner data was
/// accessed and/or modified. The handle does not inspect this inner data, so
/// any accesses to the mutable references are considered a "modify" unless
/// explicitly overwritten by [`set_modified(false)`](Self::set_modified).
#[derive(Debug, DebugPls)]
pub struct ModifyHandle<'outer, 'inner, T: ?Sized>(&'outer mut ModifyHandleInner<'inner, T>);

impl<'outer, 'inner, T: ?Sized> AsRef<ModifyHandleInner<'inner, T>>
    for ModifyHandle<'outer, 'inner, T>
{
    fn as_ref(&self) -> &ModifyHandleInner<'inner, T> {
        &self.0
    }
}

impl<'outer, 'inner, T: ?Sized> AsMut<ModifyHandleInner<'inner, T>>
    for ModifyHandle<'outer, 'inner, T>
{
    fn as_mut(&mut self) -> &mut ModifyHandleInner<'inner, T> {
        self.0
    }
}

impl<'outer, 'inner, T: ?Sized> Deref for ModifyHandle<'outer, 'inner, T> {
    type Target = ModifyHandleInner<'inner, T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'outer, 'inner, T: ?Sized> DerefMut for ModifyHandle<'outer, 'inner, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
