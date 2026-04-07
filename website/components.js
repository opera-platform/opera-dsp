// Renders shared navigation and footer across every page from a single config source.
(function () {
  var site = window.OPERA_SITE;
  var themeApi = window.OPERA_THEME;
  var currentPage = getCurrentPage();
  var githubIcon = '<svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><path d="M12 0C5.373 0 0 5.373 0 12c0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23A11.46 11.46 0 0 1 12 5.8c1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576C20.566 21.8 24 17.302 24 12 24 5.373 18.627 0 12 0Z"></path></svg>';
  var themeIcons = '<svg class="icon-moon" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><path d="M21 12.79A9 9 0 1111.21 3 7 7 0 0021 12.79z"></path></svg><svg class="icon-sun" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><circle cx="12" cy="12" r="5"></circle><line x1="12" y1="1" x2="12" y2="3"></line><line x1="12" y1="21" x2="12" y2="23"></line><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"></line><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"></line><line x1="1" y1="12" x2="3" y2="12"></line><line x1="21" y1="12" x2="23" y2="12"></line><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"></line><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"></line></svg>';

  if (!site) {
    return;
  }

  function getCurrentPage() {
    var path = window.location.pathname.split('/').pop();
    return path || 'index.html';
  }

  function createElement(tagName, className, text) {
    var element = document.createElement(tagName);

    if (className) {
      element.className = className;
    }

    if (typeof text === 'string') {
      element.textContent = text;
    }

    return element;
  }

  function createLogoLink(height) {
    var link = createElement('a', 'nav-logo');
    var lightLogo = createElement('img', 'logo-light');
    var darkLogo = createElement('img', 'logo-dark');

    link.href = 'index.html';
    lightLogo.src = 'images/logo-light.svg';
    lightLogo.alt = 'OPERA-DSP';
    lightLogo.height = height;
    darkLogo.src = 'images/logo-dark.svg';
    darkLogo.alt = 'OPERA-DSP';
    darkLogo.height = height;

    link.appendChild(lightLogo);
    link.appendChild(darkLogo);

    return link;
  }

  function createThemeToggleButton() {
    var button = createElement('button', 'theme-toggle');

    button.type = 'button';
    button.setAttribute('aria-label', 'Toggle theme');
    button.innerHTML = themeIcons;
    button.addEventListener('click', function () {
      if (themeApi && typeof themeApi.toggleTheme === 'function') {
        themeApi.toggleTheme();
      }
    });

    return button;
  }

  function applyLinkState(link, href) {
    if (href === currentPage) {
      link.setAttribute('aria-current', 'page');
    }
  }

  function createPageLink(item) {
    var link = createElement('a', '', item.label);

    link.href = item.href;
    applyLinkState(link, item.href);

    return link;
  }

  function createGitHubLink(className, withIcon) {
    var link = createElement('a', className, withIcon ? null : 'GitHub');

    link.href = site.repoUrl;
    link.target = '_blank';
    link.rel = 'noreferrer';

    if (withIcon) {
      link.innerHTML = githubIcon + '<span>GitHub</span>';
    }

    return link;
  }

  function renderNav() {
    var fragment = document.createDocumentFragment();
    var nav = createElement('nav', 'nav');
    var navInner = createElement('div', 'nav-inner');
    var navLinks = createElement('div', 'nav-links');
    var menuToggle = createElement('button', 'nav-toggle');
    var mobileMenu = createElement('div', 'mobile-menu');

    nav.setAttribute('aria-label', 'Primary');
    menuToggle.type = 'button';
    menuToggle.setAttribute('aria-expanded', 'false');
    menuToggle.setAttribute('aria-controls', 'mobile-menu');
    menuToggle.setAttribute('aria-label', 'Open menu');
    menuToggle.appendChild(createElement('span'));
    menuToggle.appendChild(createElement('span'));

    mobileMenu.id = 'mobile-menu';
    mobileMenu.hidden = true;

    // The shared nav is assembled in JS so all entry pages stay thin and
    // changes to navigation only need to happen in one place.
    navInner.appendChild(createLogoLink(28));

    site.navigation.forEach(function (item) {
      navLinks.appendChild(createPageLink(item));
    });

    navLinks.appendChild(createGitHubLink('nav-github', true));
    navLinks.appendChild(createThemeToggleButton());
    navInner.appendChild(navLinks);
    navInner.appendChild(menuToggle);
    nav.appendChild(navInner);

    site.navigation.forEach(function (item) {
      mobileMenu.appendChild(createPageLink(item));
    });

    mobileMenu.appendChild(createGitHubLink('', false));
    mobileMenu.appendChild(createThemeToggleButton());

    fragment.appendChild(nav);
    fragment.appendChild(mobileMenu);

    initMobileMenu(nav, menuToggle, mobileMenu);

    return fragment;
  }

  function renderFooter() {
    var footer = createElement('footer', 'footer');
    var inner = createElement('div', 'footer-inner');
    var footerLeft = createElement('div', 'footer-left');
    var footerRight = createElement('div', 'footer-right');
    var lightLogo = createElement('img', 'logo-light');
    var darkLogo = createElement('img', 'logo-dark');

    lightLogo.src = 'images/logo-light.svg';
    lightLogo.alt = 'OPERA-DSP';
    lightLogo.height = 18;
    darkLogo.src = 'images/logo-dark.svg';
    darkLogo.alt = 'OPERA-DSP';
    darkLogo.height = 18;

    footerLeft.appendChild(lightLogo);
    footerLeft.appendChild(darkLogo);

    site.navigation.forEach(function (item) {
      footerRight.appendChild(createPageLink(item));
    });
    footerRight.appendChild(createGitHubLink('', false));

    inner.appendChild(footerLeft);
    inner.appendChild(footerRight);
    footer.appendChild(inner);

    return footer;
  }

  function replacePlaceholder(placeholder, content) {
    if (!placeholder || !placeholder.parentNode) {
      return;
    }

    // Insert before removing the placeholder so layout stays stable while the
    // shared shell is being attached.
    placeholder.parentNode.insertBefore(content, placeholder);
    placeholder.remove();
  }

  function initMobileMenu(nav, toggle, mobileMenu) {
    function setMenuState(isOpen) {
      toggle.classList.toggle('active', isOpen);
      mobileMenu.classList.toggle('open', isOpen);
      mobileMenu.hidden = !isOpen;
      toggle.setAttribute('aria-expanded', String(isOpen));
      toggle.setAttribute('aria-label', isOpen ? 'Close menu' : 'Open menu');
    }

    toggle.addEventListener('click', function () {
      setMenuState(!mobileMenu.classList.contains('open'));
    });

    mobileMenu.addEventListener('click', function (event) {
      // Close on navigation so the menu state does not leak across page loads.
      if (event.target.closest('a')) {
        setMenuState(false);
      }
    });

    document.addEventListener('keydown', function (event) {
      if (event.key === 'Escape') {
        setMenuState(false);
      }
    });

    document.addEventListener('click', function (event) {
      if (!nav.contains(event.target) && !mobileMenu.contains(event.target)) {
        setMenuState(false);
      }
    });

    window.addEventListener('resize', function () {
      if (window.innerWidth > 640) {
        setMenuState(false);
      }
    });
  }

  replacePlaceholder(document.getElementById('nav-placeholder'), renderNav());
  replacePlaceholder(document.getElementById('footer-placeholder'), renderFooter());
})();
