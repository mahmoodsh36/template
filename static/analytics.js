// custom umami event tracking
(function () {
  'use strict';

  function track(name, data) {
    try {
      if (window.umami && typeof window.umami.track === 'function') {
        data ? window.umami.track(name, data) : window.umami.track(name);
      }
    } catch (e) { /* never let analytics break the page */ }
  }

  var pagePath = location.pathname + location.search;

  // scroll depth (25/50/75/100% milestones, once each)
  var depths = [25, 50, 75, 100];
  var hit = {};
  var maxDepth = 0;

  function scrollPct() {
    var doc = document.documentElement;
    var scrollable = doc.scrollHeight - window.innerHeight;
    if (scrollable <= 0) return 100;
    return Math.min(100, Math.round((window.scrollY / scrollable) * 100));
  }

  var scrollScheduled = false;
  function onScroll() {
    if (scrollScheduled) return;
    scrollScheduled = true;
    window.requestAnimationFrame(function () {
      scrollScheduled = false;
      var pct = scrollPct();
      if (pct > maxDepth) maxDepth = pct;
      depths.forEach(function (d) {
        if (pct >= d && !hit[d]) {
          hit[d] = true;
          track('scroll-depth', { percent: d, path: pagePath });
        }
      });
    });
  }
  window.addEventListener('scroll', onScroll, { passive: true });

  // click tracking via delegation (outbound, internal, and known UI)
  document.addEventListener('click', function (e) {
    var el = e.target && e.target.closest ? e.target.closest('a, button') : null;
    if (!el) return;

    // links
    if (el.tagName === 'A' && el.href) {
      var url;
      try { url = new URL(el.href); } catch (_) { url = null; }
      if (url && url.host && url.host !== location.host) {
        track('outbound-link', { url: el.href, text: (el.textContent || '').trim().slice(0, 80) });
      } else if (url) {
        track('internal-link', { path: url.pathname + url.search, text: (el.textContent || '').trim().slice(0, 80) });
      }
      return;
    }

    // known UI buttons (ids/classes come from main.js handlers)
    if (el.id === 'themeToggle') {
      track('theme-toggle', { to: document.body.classList.contains('dark-theme') ? 'light' : 'dark' });
    } else if (el.id === 'tocToggle') {
      track('toc-toggle');
    } else if (el.classList.contains('filter-btn')) {
      track('archive-filter', { value: (el.textContent || '').trim().slice(0, 50) });
    } else if (el.classList.contains('category-btn')) {
      track('category-filter', { value: (el.textContent || '').trim().slice(0, 50) });
    }
  }, true);

  // search usage (debounced, fires once per search burst)
  var searchTimer;
  document.addEventListener('input', function (e) {
    var t = e.target;
    if (!t || t.id !== 'searchBar') return;
    clearTimeout(searchTimer);
    searchTimer = setTimeout(function () {
      var q = (t.value || '').trim();
      if (q) track('search', { length: q.length });
    }, 1200);
  });

  // text copy (e.g. copying code snippets / quotes)
  document.addEventListener('copy', function () {
    var len = 0;
    try { len = (window.getSelection().toString() || '').length; } catch (_) {}
    track('copy-text', { length: len, path: pagePath });
  });

  // engagement: time on page + max scroll, sent once on exit
  var start = Date.now();
  var sentExit = false;
  function sendExit() {
    if (sentExit) return;
    sentExit = true;
    var seconds = Math.round((Date.now() - start) / 1000);
    track('page-engagement', { seconds: seconds, maxScroll: maxDepth, path: pagePath });
  }
  document.addEventListener('visibilitychange', function () {
    if (document.visibilityState === 'hidden') sendExit();
  });
  window.addEventListener('pagehide', sendExit);
})();
