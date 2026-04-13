// nav-buttons.js - Automatic Previous / Next buttons placed in nav-path (bottom)

document.addEventListener('DOMContentLoaded', function() {
    const interval = setInterval(() => {
        // Check if the DOM element exists
        const nav = document.querySelector('#nav-tree-contents')
        if (nav) {
            clearInterval(interval); // Stop the interval
            const treeLinks = Array.from(nav.querySelectorAll("ul a[class$='.html']"));
            // Get current page URL
            let currentUrl = window.location.pathname.replace(/^.*[\\/]/, '');
            if (currentUrl.endsWith('/')) currentUrl += 'index.html';

            // Find current index
            let currentIndex = -1;
            for (let i = 0; i < treeLinks.length; i++) {
                let href = treeLinks[i].getAttribute('href');
                if (!href) continue;

                let linkUrl = new URL(href, window.location.origin).pathname.replace(/^.*[\\/]/, '');
                if (linkUrl.endsWith('/')) linkUrl += 'index.html';

                if (linkUrl === currentUrl) {
                    currentIndex = i;
                    break;
                }
            }

            if (currentIndex === -1) {
                console.error("Problem with the current index");
                return
            };

            const prev = currentIndex > 0 ? treeLinks[currentIndex - 1] : null;
            const next = currentIndex < treeLinks.length - 1 ? treeLinks[currentIndex + 1] : null;

            const navHTML = `
                <div class="section_buttons">
                    <table class="markdownTable">
                        <tbody>
                            <tr class="markdownTableHead">
                                <th class="markdownTableHeadLeft">Previous  </th>
                                <th class="markdownTableHeadRight">Next  </th>
                            </tr>
                            <tr class="markdownTableRowOdd">
                                <td class="markdownTableBodyLeft">
                                    ${prev ? `
                                        <a href="${prev.getAttribute('href')}">
                                            ${prev.textContent.trim()}
                                        </a>
                                    ` : ''}
                                </td>
                                <td class="markdownTableBodyRight">
                                    ${next ? `
                                        <a href="${next.getAttribute('href')}">
                                            ${next.textContent.trim()}
                                         </a>
                                    ` : ''}
                                </td>
                            </tr>
                        </tbody>
                    </table>
                </div>
            `;
            // Insert into nav-path (the breadcrumb/footer area)
            const docContent = document.querySelector('#doc-content') || 
                       document.querySelector('.contents') || 
                       document.querySelector('#main-content');
            if (docContent) {
                docContent.insertAdjacentHTML('beforeend', navHTML);
            }
        }
    }, 100); // Check every 100 milliseconds
});

function createElementFromHTML(htmlString) {
  var div = document.createElement('div');
  div.innerHTML = htmlString.trim();

  // Change this to div.childNodes to support multiple top-level nodes.
  return div.firstChild;
}