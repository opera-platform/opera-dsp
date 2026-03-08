// Set theme immediately to prevent flash
(function() {
  var theme = 'light';
  try { theme = localStorage.getItem('theme') || 'light'; } catch(e) {}
  if (theme !== 'dark' && theme !== 'light') theme = 'light';
  if (theme === 'light' && window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    try { if (!localStorage.getItem('theme')) theme = 'dark'; } catch(e) {}
  }
  document.documentElement.setAttribute('data-theme', theme);
})();

function toggleTheme() {
  var current = document.documentElement.getAttribute('data-theme') || 'light';
  var next = current === 'dark' ? 'light' : 'dark';
  document.documentElement.setAttribute('data-theme', next);
  try { localStorage.setItem('theme', next); } catch(e) {}
}
