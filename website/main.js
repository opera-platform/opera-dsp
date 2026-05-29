// Initializes page-level behavior for the static site.
(function () {
  var site = window.OPERA_SITE;
  var copyIcon = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><rect x="9" y="9" width="13" height="13" rx="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg>';
  var successIcon = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><path d="M20 6 9 17l-5-5"></path></svg>';
  var errorIcon = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" aria-hidden="true"><circle cx="12" cy="12" r="10"></circle><line x1="15" y1="9" x2="9" y2="15"></line><line x1="9" y1="9" x2="15" y2="15"></line></svg>';
  var copyResetDelayMs = 2000;

  // Starts shared behavior; page-specific initializers return when their DOM is absent.
  function init() {
    applySiteContent();
    initCopyButtons();
    initScrollReveal();
    initNewsPage();
    initDocsPage();
  }

  // Copies shared project metadata from site-config.js into opted-in elements.
  function applySiteContent() {
    if (!site) {
      return;
    }

    // Let HTML opt into shared project metadata with data attributes instead of
    // duplicating values in multiple templates.
    updateTextContent('[data-site-clone-command]', site.cloneCommand);
    updateLinkHref('[data-site-repo-link]', site.repoUrl);
  }

  // Updates every matching node with plain text from the shared config.
  function updateTextContent(selector, value) {
    document.querySelectorAll(selector).forEach(function (element) {
      element.textContent = value;
    });
  }

  // Applies external-link attributes consistently to generated project links.
  function updateLinkHref(selector, href) {
    document.querySelectorAll(selector).forEach(function (link) {
      link.href = href;
      setExternalLink(link);
    });
  }

  // Marks links that should open outside the current static page.
  function setExternalLink(link) {
    link.target = '_blank';
    link.rel = 'noopener noreferrer';
  }

  // Wires copy buttons to either a site-config key or explicit text.
  function initCopyButtons() {
    document.querySelectorAll('[data-copy-source], [data-copy-text]').forEach(function (button) {
      setCopyButtonState(button, 'idle');
      button.addEventListener('click', function () {
        var text = resolveCopyText(button);

        if (!text) {
          setCopyButtonState(button, 'error');
          return;
        }

        copyText(text)
          .then(function () {
            setCopyButtonState(button, 'success');
          })
          .catch(function () {
            setCopyButtonState(button, 'error');
          });
      });
    });
  }

  // Resolves the text a copy button should write to the clipboard.
  function resolveCopyText(button) {
    var source = button.getAttribute('data-copy-source');

    if (source && site && typeof site[source] === 'string') {
      return site[source];
    }

    return button.getAttribute('data-copy-text') || '';
  }

  // Uses the modern clipboard API and falls back for older browsers.
  function copyText(text) {
    if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
      return navigator.clipboard.writeText(text).catch(function () {
        return fallbackCopyText(text);
      });
    }

    return fallbackCopyText(text);
  }

  // Copies text through a temporary textarea when navigator.clipboard is unavailable.
  function fallbackCopyText(text) {
    return new Promise(function (resolve, reject) {
      var textArea = document.createElement('textarea');

      // Keep the fallback off-screen and temporary so it does not affect layout
      // or leave behind focusable DOM once the copy attempt finishes.
      textArea.value = text;
      textArea.setAttribute('readonly', '');
      textArea.style.position = 'fixed';
      textArea.style.top = '-9999px';
      document.body.appendChild(textArea);
      textArea.select();

      try {
        if (!document.execCommand('copy')) {
          throw new Error('Copy command was rejected.');
        }

        resolve();
      } catch (error) {
        reject(error);
      } finally {
        textArea.remove();
      }
    });
  }

  // Updates copy button icon, accessible label, and short-lived success/error state.
  function setCopyButtonState(button, state) {
    var labels = {
      idle: 'Copy install command',
      success: 'Copied install command',
      error: 'Unable to copy install command'
    };
    var icons = {
      idle: copyIcon,
      success: successIcon,
      error: errorIcon
    };

    button.innerHTML = icons[state] || copyIcon;
    button.setAttribute('aria-label', labels[state] || labels.idle);
    button.title = labels[state] || labels.idle;

    if (button._copyResetTimer) {
      window.clearTimeout(button._copyResetTimer);
    }

    if (state !== 'idle') {
      button._copyResetTimer = window.setTimeout(function () {
        setCopyButtonState(button, 'idle');
      }, copyResetDelayMs);
    }
  }

  // Reveals home-page sections once as they enter the viewport.
  function initScrollReveal() {
    var targets = Array.prototype.slice.call(document.querySelectorAll('.feature-card, .code-block, .funding-panel, .cta'));

    if (!targets.length) {
      return;
    }

    if (!('IntersectionObserver' in window)) {
      targets.forEach(function (element) {
        element.classList.add('visible');
      });
      return;
    }

    var observer = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        if (!entry.isIntersecting) {
          return;
        }

        // These reveals only need to happen once, so stop observing after the
        // element becomes visible.
        entry.target.classList.add('visible');
        observer.unobserve(entry.target);
      });
    }, { threshold: 0.1 });

    targets.forEach(function (element) {
      observer.observe(element);
    });
  }

  // Loads structured news content when the news page container exists.
  function initNewsPage() {
    var container = document.getElementById('news-list');

    if (!container || !site) {
      return;
    }

    fetchJson(site.paths.newsFeed)
      .then(function (items) {
        renderNewsItems(container, items);
      })
      .catch(function () {
        renderStatusMessage(container, 'Failed to load news. Please try refreshing the page.');
      });
  }

  // Renders news items into a fragment to avoid repeated live DOM writes.
  function renderNewsItems(container, items) {
    var fragment = document.createDocumentFragment();

    items.forEach(function (item) {
      fragment.appendChild(createNewsItem(item));
    });

    container.replaceChildren(fragment);
  }

  // Builds one news article from a news.json item.
  function createNewsItem(item) {
    var article = createElement('article', 'news-item');
    var date = createElement('div', 'news-date', item.date || '');
    var heading = createElement('h2');
    var link = createElement('a', '', item.title || 'Untitled update');
    var body = createElement('p', '', item.body || '');
    var tag = createElement('span', 'news-tag', item.tag || 'Update');

    link.href = item.url || site.repoUrl;
    setExternalLink(link);

    heading.appendChild(link);
    article.appendChild(date);
    article.appendChild(heading);
    article.appendChild(body);
    article.appendChild(tag);

    return article;
  }

  // Loads the manual docs manifest, sidebar, and partial HTML sections.
  function initDocsPage() {
    var sidebar = document.getElementById('docs-sidebar');
    var content = document.getElementById('docs-content');

    if (!sidebar || !content || !site) {
      return;
    }

    fetchJson(site.paths.docsManifest)
      .then(function (data) {
        renderDocsSidebar(sidebar, data.sidebar || []);

        return loadDocsSections(data.sections || []);
      })
      .then(function (fragment) {
        content.replaceChildren(fragment);
        initDocsSectionTracking(sidebar, content);
        scrollToHashTarget(content);
      })
      .catch(function () {
        renderStatusMessage(content, 'Failed to load documentation. Please try refreshing the page.');
      });
  }

  // Builds grouped docs navigation from docs.json.
  function renderDocsSidebar(sidebar, groups) {
    var fragment = document.createDocumentFragment();

    groups.forEach(function (group) {
      var groupElement = createElement('div', 'docs-sidebar-group');
      var label = createElement('div', 'docs-sidebar-label', group.label || '');

      groupElement.appendChild(label);

      (group.links || []).forEach(function (item) {
        var link = createElement('a', '', item.title || '');

        link.href = item.href || '#';

        if (isExternalDocsLink(item.href)) {
          setExternalLink(link);
        }

        groupElement.appendChild(link);
      });

      fragment.appendChild(groupElement);
    });

    sidebar.replaceChildren(fragment);
  }

  // Treats non-hash docs links as separate generated documentation pages.
  function isExternalDocsLink(href) {
    return typeof href === 'string' && href.indexOf('#') !== 0;
  }

  // Fetches hand-authored docs partials and returns them as a single fragment.
  function loadDocsSections(sectionNames) {
    return Promise.all(sectionNames.map(function (name) {
      return fetchText(site.paths.docsSectionsDir + name + '.html');
    })).then(function (htmlParts) {
      var fragment = document.createDocumentFragment();
      var wrapper = createElement('div');

      // Parse all partials in a detached node first, then move the resulting
      // elements into the live container in a single pass.
      wrapper.innerHTML = htmlParts.join('\n');
      applyExternalLinkAttrs(wrapper);

      while (wrapper.firstChild) {
        fragment.appendChild(wrapper.firstChild);
      }

      return fragment;
    });
  }

  // Adds safe external-link attributes to links that come from docs partials.
  function applyExternalLinkAttrs(root) {
    root.querySelectorAll('a[target="_blank"]').forEach(function (link) {
      setExternalLink(link);
    });
  }

  // Tracks the visible docs section and keeps the sidebar state in sync.
  function initDocsSectionTracking(sidebar, content) {
    var sectionLinks = Array.prototype.slice.call(sidebar.querySelectorAll('a[href^="#"]'));
    var linkById = {};

    if (!sectionLinks.length) {
      return;
    }

    sectionLinks.forEach(function (link) {
      var id = link.getAttribute('href').slice(1);

      linkById[id] = link;
      link.addEventListener('click', function () {
        setActiveDocsLink(linkById, id);
      });
    });

    var sections = sectionLinks.map(function (link) {
      return content.querySelector(link.getAttribute('href'));
    }).filter(Boolean);

    // Makes direct hash navigation highlight the matching sidebar link.
    function activateFromHash() {
      var id = window.location.hash.replace(/^#/, '');

      if (id && linkById[id]) {
        setActiveDocsLink(linkById, id);
        return;
      }

      if (sections[0]) {
        setActiveDocsLink(linkById, sections[0].id);
      }
    }

    if ('IntersectionObserver' in window) {
      var observer = new IntersectionObserver(function (entries) {
        var visibleEntries = entries.filter(function (entry) {
          return entry.isIntersecting;
        }).sort(function (left, right) {
          return left.boundingClientRect.top - right.boundingClientRect.top;
        });

        // When multiple sections overlap the viewport, prefer the one nearest
        // the top so the sidebar state matches what readers are focused on.
        if (visibleEntries[0]) {
          setActiveDocsLink(linkById, visibleEntries[0].target.id);
        }
      }, {
        rootMargin: '-120px 0px -55% 0px',
        threshold: [0.1, 0.5, 1]
      });

      sections.forEach(function (section) {
        observer.observe(section);
      });
    }

    window.addEventListener('hashchange', activateFromHash);
    activateFromHash();
  }

  // Marks one docs sidebar link active and clears the rest.
  function setActiveDocsLink(linkById, activeId) {
    Object.keys(linkById).forEach(function (id) {
      linkById[id].classList.toggle('active', id === activeId);
    });
  }

  // Scrolls to an initial hash after async docs sections have been inserted.
  function scrollToHashTarget(content) {
    if (!window.location.hash) {
      return;
    }

    var target = content.querySelector(window.location.hash);

    if (target) {
      target.scrollIntoView();
    }
  }

  // Replaces a dynamic container with a concise loading/error status.
  function renderStatusMessage(container, message) {
    var status = createElement('p', 'page-status', message);

    container.replaceChildren(status);
  }

  // Fetches and parses JSON with one shared response check.
  function fetchJson(path) {
    return fetchResource(path, 'json');
  }

  // Fetches text/HTML partials with one shared response check.
  function fetchText(path) {
    return fetchResource(path, 'text');
  }

  // Revalidates dynamic JSON/HTML partials so docs/news updates are visible.
  function fetchResource(path, responseType) {
    return fetch(path, { cache: 'no-cache' }).then(function (response) {
      if (!response.ok) {
        throw new Error(response.statusText);
      }

      return response[responseType]();
    });
  }

  // Small DOM helper used by generated page content.
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

  init();
})();
