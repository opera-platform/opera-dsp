// Page-specific functionality for index, news, and docs pages.
// Each feature is wrapped in an IIFE so it only runs when its target elements exist.

// Copies the git clone command to clipboard and shows a check mark for ~2 seconds (index.html)
function copyInstall(btn) {
  navigator.clipboard.writeText('git clone https://github.com/opera-platform/opera-dsp.git');
  btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M20 6L9 17l-5-5"/></svg>';
  setTimeout(function() {
    btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"/></svg>';
  }, 2000);
}

// Fade-in animation for feature cards, code block, and CTA when they scroll into view (see index.html)
(function() {
  var targets = document.querySelectorAll('.feature-card, .code-block, .cta');
  if (targets.length === 0) return;
  var observer = new IntersectionObserver(function(entries) {
    entries.forEach(function(e) {
      if (e.isIntersecting) e.target.classList.add('visible');
    });
  }, { threshold: 0.1 });
  targets.forEach(function(el) { observer.observe(el); });
})();

// Fetches news.json and renders each item as an article card (see news.html)
(function() {
  var container = document.getElementById('news-list');
  if (!container) return;
  fetch('news.json')
    .then(function(res) {
      if (!res.ok) throw new Error(res.statusText);
      return res.json();
    })
    .then(function(items) {
      container.innerHTML = items.map(function(item) {
        return '<article class="news-item">'
          + '<div class="news-date">' + item.date + '</div>'
          + '<h2><a href="' + item.url + '" target="_blank">' + item.title + '</a></h2>'
          + '<p>' + item.body + '</p>'
          + '<span class="news-tag">' + item.tag + '</span>'
          + '</article>';
      }).join('');
    })
    .catch(function() {
      container.innerHTML = '<p style="color:var(--text-tertiary)">Failed to load news. Please try refreshing the page.</p>';
    });
})();

// Loads the docs sidebar and content sections from docs.json and HTML partials (see docs.html)
(function() {
  var sidebar = document.getElementById('docs-sidebar');
  var content = document.getElementById('docs-content');
  if (!sidebar || !content) return;

  fetch('docs.json')
    .then(function(res) {
      if (!res.ok) throw new Error(res.statusText);
      return res.json();
    })
    .then(function(data) {
      // Build sidebar navigation from grouped links defined in docs.json
      sidebar.innerHTML = data.sidebar.map(function(group) {
        return '<div class="docs-sidebar-group">'
          + '<div class="docs-sidebar-label">' + group.label + '</div>'
          + group.links.map(function(link) {
              return '<a href="' + link.href + '">' + link.title + '</a>';
            }).join('')
          + '</div>';
      }).join('');

      // Fetch each section's HTML partial (e.g. docs-sections/overview.html) in order
      return Promise.all(data.sections.map(function(name) {
        return fetch('docs-sections/' + name + '.html')
          .then(function(res) {
            if (!res.ok) throw new Error(name + ': ' + res.statusText);
            return res.text();
          });
      }));
    })
    .then(function(htmlParts) {
      // Combine all section partials into the main content area
      content.innerHTML = htmlParts.join('\n');

      // highlights the sidebar link matching the currently visible section.
      // Throttled with requestAnimationFrame to avoid excessive reflows.
      var sidebarLinks = document.querySelectorAll('.docs-sidebar a');
      var sections = document.querySelectorAll('.docs-content [id]');
      var ticking = false;
      window.addEventListener('scroll', function() {
        if (ticking) return;
        ticking = true;
        requestAnimationFrame(function() {
          var current = '';
          sections.forEach(function(s) {
            if (window.scrollY >= s.offsetTop - 100) current = s.id;
          });
          sidebarLinks.forEach(function(a) {
            a.classList.toggle('active', a.getAttribute('href') === '#' + current);
          });
          ticking = false;
        });
      });

      // If the URL has a hash (e.g. docs.html#windowing), scroll to that section
      if (window.location.hash) {
        var target = document.querySelector(window.location.hash);
        if (target) target.scrollIntoView();
      }
    })
    .catch(function() {
      content.innerHTML = '<p style="color:var(--text-tertiary)">Failed to load documentation. Please try refreshing the page.</p>';
    });
})();
