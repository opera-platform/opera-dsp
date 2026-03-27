// main.js — Page-specific functionality

// Copy-to-clipboard (index.html)
function copyInstall(btn) {
  navigator.clipboard.writeText('git clone https://github.com/opera-platform/opera-dsp.git');
  btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M20 6L9 17l-5-5"/></svg>';
  setTimeout(function() {
    btn.innerHTML = '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"/></svg>';
  }, 2000);
}

// IntersectionObserver animations (index.html)
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

// Scroll-spy for docs sidebar (docs.html)
(function() {
  var sidebarLinks = document.querySelectorAll('.docs-sidebar a');
  if (sidebarLinks.length === 0) return;
  var sections = document.querySelectorAll('[id]');
  window.addEventListener('scroll', function() {
    var current = '';
    sections.forEach(function(s) {
      if (window.scrollY >= s.offsetTop - 100) current = s.id;
    });
    sidebarLinks.forEach(function(a) {
      a.classList.toggle('active', a.getAttribute('href') === '#' + current);
    });
  });
})();
