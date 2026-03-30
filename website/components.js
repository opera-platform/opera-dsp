// Injects shared nav, mobile menu, and footer into every page.
// Each page has #nav-placeholder and #footer-placeholder divs that get replaced
// with the actual markup on load. This avoids duplicating nav/footer HTML across pages.
(function() {

  // Desktop nav with logo (light/dark variants), page links, GitHub link, and theme toggle.
  // Mobile button is hidden on desktop via CSS, shown below 640px.
  var navHTML = `
  <nav class="nav">
    <div class="nav-inner">
      <a href="index.html" class="nav-logo">
        <img src="images/logo-light.svg" alt="OPERA-DSP" height="28" class="logo-light">
        <img src="images/logo-dark.svg" alt="OPERA-DSP" height="28" class="logo-dark">
      </a>
      <div class="nav-links">
        <a href="docs.html">Docs</a>
        <a href="news.html">News</a>
        <a href="https://github.com/opera-platform/opera-dsp" target="_blank" class="nav-github">
          <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>
          GitHub
        </a>
        <button class="theme-toggle" onclick="toggleTheme()" aria-label="Toggle theme">
          <svg class="icon-moon" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z"/></svg>
          <svg class="icon-sun" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="5"/><line x1="12" y1="1" x2="12" y2="3"/><line x1="12" y1="21" x2="12" y2="23"/><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/><line x1="1" y1="12" x2="3" y2="12"/><line x1="21" y1="12" x2="23" y2="12"/><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/></svg>
        </button>
      </div>
      <button class="nav-toggle" aria-label="Menu">
        <span></span><span></span>
      </button>
    </div>
  </nav>
  <div class="mobile-menu">
    <a href="docs.html">Docs</a>
    <a href="news.html">News</a>
    <a href="https://github.com/opera-platform/opera-dsp" target="_blank">GitHub</a>
    <button class="theme-toggle" onclick="toggleTheme()" aria-label="Toggle theme">
      <svg class="icon-moon" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z"/></svg>
      <svg class="icon-sun" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="5"/><line x1="12" y1="1" x2="12" y2="3"/><line x1="12" y1="21" x2="12" y2="23"/><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/><line x1="1" y1="12" x2="3" y2="12"/><line x1="21" y1="12" x2="23" y2="12"/><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/></svg>
    </button>
  </div>`;

  // Footer with logo and navigation links
  var footerHTML = `
  <footer class="footer">
    <div class="footer-inner">
      <div class="footer-left">
        <img src="images/logo-light.svg" alt="OPERA-DSP" height="18" class="logo-light">
        <img src="images/logo-dark.svg" alt="OPERA-DSP" height="18" class="logo-dark">
      </div>
      <div class="footer-right">
        <a href="docs.html">Docs</a>
        <a href="news.html">News</a>
        <a href="https://github.com/opera-platform/opera-dsp" target="_blank">GitHub</a>
      </div>
    </div>
  </footer>`;

  // Replace placeholder divs with actual nav/footer markup
  var navEl = document.getElementById('nav-placeholder');
  if (navEl) { navEl.outerHTML = navHTML; }

  var footerEl = document.getElementById('footer-placeholder');
  if (footerEl) { footerEl.outerHTML = footerHTML; }

  // Button toggles the mobile menu open/closed
  var toggle = document.querySelector('.nav-toggle');
  var mobileMenu = document.querySelector('.mobile-menu');
  if (toggle && mobileMenu) {
    toggle.addEventListener('click', function() {
      toggle.classList.toggle('active');
      mobileMenu.classList.toggle('open');
    });
  }
})();
