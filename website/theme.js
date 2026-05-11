// Loaded in <head> to set the theme before first paint, preventing flash of wrong theme.
(function () {
  var storageKey = 'theme';

  // Reads the saved theme without failing when storage is blocked.
  function getStoredTheme() {
    try {
      return localStorage.getItem(storageKey);
    } catch (error) {
      return null;
    }
  }

  // Uses an explicit saved choice first, then falls back to the OS preference.
  function getPreferredTheme() {
    var storedTheme = getStoredTheme();

    // An explicit user choice always wins over the OS preference.
    if (storedTheme === 'dark' || storedTheme === 'light') {
      return storedTheme;
    }

    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
      return 'dark';
    }

    return 'light';
  }

  // Applies the theme token switch before the page paints.
  function applyTheme(theme) {
    document.documentElement.setAttribute('data-theme', theme);
  }

  // Toggles the current theme and persists the user's choice when possible.
  function toggleTheme() {
    var currentTheme = document.documentElement.getAttribute('data-theme') || 'light';
    var nextTheme = currentTheme === 'dark' ? 'light' : 'dark';

    applyTheme(nextTheme);

    try {
      localStorage.setItem(storageKey, nextTheme);
    } catch (error) {
      // Ignore storage failures and keep the in-memory theme change.
    }

    return nextTheme;
  }

  window.OPERA_THEME = {
    toggleTheme: toggleTheme
  };

  applyTheme(getPreferredTheme());
})();
