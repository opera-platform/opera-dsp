// Loaded in <head> to set the theme before first paint, preventing flash of wrong theme.
(function () {
  var storageKey = 'theme';

  function getStoredTheme() {
    try {
      return localStorage.getItem(storageKey);
    } catch (error) {
      return null;
    }
  }

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

  function applyTheme(theme) {
    document.documentElement.setAttribute('data-theme', theme);
  }

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
    applyTheme: applyTheme,
    getTheme: function () {
      return document.documentElement.getAttribute('data-theme') || 'light';
    },
    toggleTheme: toggleTheme
  };

  // Keep the legacy global available because the HTML is still plain static pages.
  window.toggleTheme = toggleTheme;
  applyTheme(getPreferredTheme());
})();
