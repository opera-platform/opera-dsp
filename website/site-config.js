(function () {
  // Keep cross-page project metadata here so content updates do not require
  // hunting through multiple HTML and JS files.
  var siteConfig = {
    repoUrl: 'https://github.com/opera-platform/opera-dsp',
    cloneCommand: 'git clone https://github.com/opera-platform/opera-dsp.git',
    paths: {
      docsManifest: 'docs.json',
      docsSectionsDir: 'docs-sections/',
      newsFeed: 'news.json'
    },
    navigation: [
      { href: 'docs.html', label: 'Docs' },
      { href: 'news.html', label: 'News' }
    ]
  };

  // Freeze the public config to avoid accidental mutation from page scripts.
  window.OPERA_SITE = Object.freeze(siteConfig);
})();
