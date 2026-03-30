// Loaded in <head> to set the theme before first paint, preventing flash of wrong theme.
(function() {
  // Read saved preference, default to light
  var theme = 'light';
  try { theme = localStorage.getItem('theme') || 'light'; } catch(e) {}
  if (theme !== 'dark' && theme !== 'light') theme = 'light';

  // If no saved preference, respect OS-level dark mode setting
  if (theme === 'light' && window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    try { if (!localStorage.getItem('theme')) theme = 'dark'; } catch(e) {}
  }

  // Apply theme via data attribute (drives CSS variable switching)
  document.documentElement.setAttribute('data-theme', theme);
})();

// Toggles between light and dark theme, persists choice to localStorage
function toggleTheme() {
  var current = document.documentElement.getAttribute('data-theme') || 'light';
  var next = current === 'dark' ? 'light' : 'dark';
  document.documentElement.setAttribute('data-theme', next);
  try { localStorage.setItem('theme', next); } catch(e) {}
}
