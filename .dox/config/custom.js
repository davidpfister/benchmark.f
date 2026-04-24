document.addEventListener("DOMContentLoaded", () => {
  waitForNavTree();
});

function waitForNavTree() {
  const interval = setInterval(() => {
    const navTree = document.getElementById("nav-tree");

    if (!navTree) return;

    const items = navTree.querySelectorAll(".item");
    if (items.length === 0) return;

    clearInterval(interval);

    injectVitePressHeader(navTree);
  }, 100);
}

function injectVitePressHeader(navTree) {
  if (document.getElementById("vp-header")) return;

  const header = document.createElement("header");
  header.id = "vp-header";
  const githubUrl = window.DOXYGEN_CONFIG?.github || "";
  const projectName = window.DOXYGEN_CONFIG?.projectName || "";
  const projectNumber = window.DOXYGEN_CONFIG?.projectNumber || "";
  const projectBrief = window.DOXYGEN_CONFIG?.projectBrief || "";

  header.innerHTML = `
    <div class="vp-left">
      <img src="logo.png" class="vp-logo" />
      ${
      projectName, projectBrief, projectNumber
        ?`<div><div><a href="index.html">${projectName}</a>&#160;<span id="projectnumber">v${projectNumber}</span></div><div id="projectbriefContainer"><span  id="projectbrief">${projectBrief}</span></div></div>`:""
      }
    </div>

    <div class="vp-center">
            <div id="MSearchBox" class="MSearchBoxInactive">
        <span class="left">
          <span id="MSearchSelect" class="search-icon" onmouseover="return searchBox.OnSearchSelectShow()" onmouseout="return searchBox.OnSearchSelectHide()" tabindex="0"><span class="search-icon-dropdown"></span></span>
          <input type="text" id="MSearchField" value="" placeholder="Search" accesskey="S" onfocus="searchBox.OnSearchFieldFocus(true)" onblur="searchBox.OnSearchFieldFocus(false)" onkeyup="searchBox.OnSearchFieldChange(event)">
          </span><span class="right">
            <a id="MSearchClose" href="javascript:searchBox.CloseResultsWindow()" style="display: none;"><div id="MSearchCloseImg" class="close-icon"></div></a>
          </span>
        </div>
    </div>

    <nav class="vp-nav" id="vp-nav"></nav>

    <div class="vp-right">
    <doxygen-awesome-dark-mode-toggle title="Toggle Light/Dark Mode"><svg xmlns="http://www.w3.org/2000/svg" enable-background="new 0 0 24 24" height="24px" viewBox="0 0 24 24" width="24px" fill="#FE9700"><rect fill="none" height="24" width="24"></rect><path d="M9.37,5.51C9.19,6.15,9.1,6.82,9.1,7.5c0,4.08,3.32,7.4,7.4,7.4c0.68,0,1.35-0.09,1.99-0.27 C17.45,17.19,14.93,19,12,19c-3.86,0-7-3.14-7-7C5,9.07,6.81,6.55,9.37,5.51z" opacity=".3"></path><path d="M9.37,5.51C9.19,6.15,9.1,6.82,9.1,7.5c0,4.08,3.32,7.4,7.4,7.4c0.68,0,1.35-0.09,1.99-0.27C17.45,17.19,14.93,19,12,19 c-3.86,0-7-3.14-7-7C5,9.07,6.81,6.55,9.37,5.51z M12,3c-4.97,0-9,4.03-9,9s4.03,9,9,9s9-4.03,9-9c0-0.46-0.04-0.92-0.1-1.36 c-0.98,1.37-2.58,2.26-4.4,2.26c-2.98,0-5.4-2.42-5.4-5.4c0-1.81,0.89-3.42,2.26-4.4C12.92,3.04,12.46,3,12,3L12,3z"></path></svg></doxygen-awesome-dark-mode-toggle>
      ${
      githubUrl
        ? `<a id="vp-github" class="vp-icon-button" href="${githubUrl}" target="_blank" rel="noopener">
            <svg viewBox="0 0 24 24" aria-hidden="true">
              <path fill="currentColor"
                d="M12 0C5.37 0 0 5.37 0 12a12 12 0 0 0 8.2 11.4c.6.1.82-.26.82-.58v-2.02c-3.34.72-4.04-1.6-4.04-1.6-.54-1.36-1.32-1.72-1.32-1.72-1.08-.74.08-.72.08-.72 1.2.08 1.84 1.24 1.84 1.24 1.06 1.82 2.78 1.3 3.46 1 .1-.78.42-1.3.76-1.6-2.66-.3-5.46-1.34-5.46-5.96 0-1.32.46-2.4 1.24-3.26-.12-.3-.54-1.52.12-3.18 0 0 1-.32 3.3 1.24a11.4 11.4 0 0 1 6 0C17.3 4.7 18.3 5 18.3 5c.66 1.66.24 2.88.12 3.18.78.86 1.24 1.94 1.24 3.26 0 4.64-2.8 5.66-5.48 5.96.44.38.82 1.12.82 2.26v3.36c0 .32.22.7.82.58A12 12 0 0 0 24 12c0-6.63-5.37-12-12-12z"/>
            </svg>
          </a>`
        : ""
    }
      <button id="vp-toggle-sidebar" class="vp-icon-button" aria-label="Toggle sidebar">
        <svg viewBox="0 0 24 24">
            <path fill="currentColor"
                d="M5 7h14v2H5zm0 5h14v2H5zm0 5h14v2H5z"/>
        </svg>
      </button>
    </div>
  `;

  document.body.prepend(header);

  buildTopNavigation(navTree);
  setupSidebarToggle();
  injectResizeHandle();
  restoreSidebarWidth();
  initSidebarResize();
}

function buildTopNavigation(navTree) {
  const nav = document.getElementById("vp-nav");
  if (!nav) return;

  const seen = new Set();

  // STEP 1: get all SECOND-LEVEL containers
  const level2Lists = navTree.querySelectorAll("#nav-tree-contents > ul > li > ul.children_ul");

  level2Lists.forEach(ul => {
    const items = ul.querySelectorAll(":scope > li > .item");

    items.forEach(item => {
      const labelLink = item.querySelector(".label a");
      if (!labelLink) return;

      const href = labelLink.getAttribute("href");
      const text = labelLink.innerText.trim();

      if (!href || !text) return;

      const key = href;
      if (seen.has(key)) return;
      seen.add(key);

      const a = document.createElement("a");
      a.href = href;
      a.textContent = text;

      nav.appendChild(a);
    });
  });
}

function extractLabel(item) {
  const label = item.querySelector(".label");

  if (label) {
    return label.innerText.trim();
  }

  const link = item.querySelector("a");
  if (link?.innerText?.trim()) {
    return link.innerText.trim();
  }

  return item.textContent.trim();
}

function getNodeDepth(li) {
  let depth = 0;
  let parent = li;

  while (parent) {
    if (parent.id === "nav-tree") break;

    if (parent.classList?.contains("children_ul")) {
      depth++;
    }

    parent = parent.parentElement;
  }

  return depth;
}

function setupSidebarToggle() {
  const btn = document.getElementById("vp-toggle-sidebar");
  const navTree = document.getElementById("nav-tree");

  if (!btn || !navTree) return;

  btn.addEventListener("click", () => {
    const collapsed = document.body.classList.toggle("vp-sidebar-collapsed");

    localStorage.setItem("vp-sidebar-collapsed", collapsed);
  });

  // restore state
  if (localStorage.getItem("vp-sidebar-collapsed") === "true") {
    document.body.classList.add("vp-sidebar-collapsed");
  }
}

function injectResizeHandle() {
  const navTree = document.getElementById("nav-tree");
  if (!navTree) return;

  if (document.getElementById("vp-resizer")) return;

  const handle = document.createElement("div");
  handle.id = "vp-resizer";

  navTree.appendChild(handle);
}

function initSidebarResize() {
  const handle = document.getElementById("vp-resizer");
  const navTree = document.getElementById("nav-tree");

  if (!handle || !navTree) return;

  let isDragging = false;

  handle.addEventListener("mousedown", (e) => {
    isDragging = true;
    e.preventDefault();

    document.body.classList.add("vp-resizing");
  });

  document.addEventListener("mousemove", (e) => {
    if (!isDragging) return;

    const rect = navTree.getBoundingClientRect();

    let newWidth = e.clientX - rect.left;

    const min = 180;
    const max = 600;

    if (newWidth < min) newWidth = min;
    if (newWidth > max) newWidth = max;

    document.documentElement.style.setProperty(
      "--side-nav-fixed-width",
      newWidth + "px"
    );

    localStorage.setItem("vp-sidebar-width", newWidth);
  });

  document.addEventListener("mouseup", () => {
    if (!isDragging) return;

    isDragging = false;
    document.body.classList.remove("vp-resizing");
  });
}

function restoreSidebarWidth() {
  const saved = localStorage.getItem("vp-sidebar-width");

  if (saved) {
    document.documentElement.style.setProperty(
      "--side-nav-fixed-width",
      saved + "px"
    );
  }
}