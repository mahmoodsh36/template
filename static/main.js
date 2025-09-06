// =================================
// COMMON: THEME TOGGLE FUNCTIONALITY
// =================================
function initializeThemeToggle() {
    const themeToggle = document.getElementById('themeToggle');
    const body = document.body;

    if (!themeToggle) return;

    const themeIcon = themeToggle.querySelector('i');

    // function to set theme and update everything
    function setTheme(theme) {
        body.classList.remove('dark-theme', 'light-theme');
        body.classList.add(theme);
        localStorage.setItem('theme', theme);
        updateThemeIcon(theme);

        // Reapply org-block styling when theme changes
        initializeOrgBlocks();
    }

    // function to get current theme
    function getCurrentTheme() {
        if (body.classList.contains('dark-theme')) return 'dark-theme';
        if (body.classList.contains('light-theme')) return 'light-theme';
        return null;
    }

    // check for saved theme preference or default to system preference
    function initializeTheme() {
        const savedTheme = localStorage.getItem('theme');
        if (savedTheme && (savedTheme === 'dark-theme' || savedTheme === 'light-theme')) {
            setTheme(savedTheme);
        } else {
            const defaultTheme = 'dark-theme';
            setTheme(defaultTheme);
        }
    }

    function updateThemeIcon(theme) {
        if (theme === 'dark-theme') {
            themeIcon.className = 'fas fa-sun';
        } else {
            themeIcon.className = 'fas fa-moon';
        }
    }

    // initialize theme on load
    initializeTheme();

    themeToggle.addEventListener('click', () => {
        const currentTheme = getCurrentTheme();

        if (currentTheme === 'dark-theme') {
            setTheme('light-theme');
        } else {
            setTheme('dark-theme');
        }
    });
}

// =================================
// COMMON: SEARCH FUNCTIONALITY
// =================================
let searchData = [];

// Fetch search data from search.json
async function loadSearchData() {
    try {
        const response = await fetch('/search.json');
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        searchData = await response.json();
        console.log('Search data loaded:', searchData);
    } catch (error) {
        console.error('Error loading search data:', error);
    }
}

// Search function
function performSearch(query) {
    const resultsContainer = document.getElementById("search-results-container");
    if (!resultsContainer) return;

    resultsContainer.innerHTML = '';

    if (!query) {
        // Update numbers even when query is empty
        updateSearchNumbers(0, searchData.length);
        return;
    }

    const matchingEntries = searchData.filter(entry => {
        const title = entry.title || '';
        const id = entry.id || '';
        return title.toLowerCase().includes(query.toLowerCase()) ||
               id.toLowerCase().includes(query.toLowerCase());
    });

    matchingEntries.forEach(entry => {
        const entryText = entry.title || entry.id || 'Untitled';

        const container = document.createElement("div");
        const subcontainer = document.createElement("div");
        const span = document.createElement("span");
        const plusMinusButton = document.createElement("div");
        const infoElm = document.createElement("div");

        container.className = 'search-result-container';
        plusMinusButton.className = 'plus-button';
        infoElm.className = 'info';
        subcontainer.className = 'search-result';

        // on-demand info of reference/page/whatever
        plusMinusButton.onclick = function() {
            // so that we dont insert duplicate info
            infoElm.innerHTML = '';

            const isPlus = plusMinusButton.classList.contains('plus-button');

            if (isPlus) {
                plusMinusButton.className = 'minus-button';
                fetch(entry.filepath).then(response => response.text()).then(function(text) {
                    // parse the "other" page (page containing the destination entry)
                    const page = new DOMParser().parseFromString(text, "text/html");
                    // the actual html entry from the other page
                    const docElm = page.getElementById(entry.id);
                    // the type of the entry

                    // direct link to the entry in its parent page
                    let mylink = entry['filepath'];
                    if (docElm !== null)
                        mylink = mylink + '#' + entry.id;
                    const linkElm = document.createElement('a');
                    linkElm.href = mylink;
                    linkElm.innerHTML = 'direct link';

                    const topRow = document.createElement('div');
                    topRow.className = 'separated-row';

                    // insert the info
                    let mytype = 'document';
                    if (mytype)
                        topRow.appendChild(document.createTextNode('type: ' + mytype));
                    else
                        topRow.appendChild(document.createTextNode('empty'));

                    // insert the on-demand info elements into the dom
                    topRow.appendChild(linkElm);
                    infoElm.appendChild(topRow);
                    if (docElm !== null)
                        infoElm.appendChild(docElm);
                    container.appendChild(infoElm);
                });
            } else {
                plusMinusButton.className = 'plus-button';
                if (container.querySelector('.info')) {
                    container.querySelector('.info').remove();
                }
            }
        }

        span.appendChild(document.createTextNode(entryText));
        container.appendChild(subcontainer);
        subcontainer.appendChild(span);
        subcontainer.appendChild(plusMinusButton);
        // Only append infoElm when it's actually used
        resultsContainer.appendChild(container);
    });

    // update numbers
    updateSearchNumbers(matchingEntries.length, searchData.length);
}

// Update search numbers display
function updateSearchNumbers(resultsCount, totalCount) {
    const resultsElement = document.getElementById("search-numbers-results");
    const publicElement = document.getElementById("search-numbers-public");

    if (resultsElement) resultsElement.innerHTML = resultsCount;
    if (publicElement) publicElement.innerHTML = totalCount;
}

// Handle search input
function handleSearchInput(element) {
    if (element) {
        performSearch(element.value);
    }
}

// =================================
// PAGE-SPECIFIC: ARCHIVE PAGE
// =================================
function initializeArchivePage() {
    const postsListContainer = document.getElementById('postsList');
    const archiveFiltersContainer = document.getElementById('archiveFilters');
    const archiveCounterElement = document.getElementById('archiveCounter');
    if (!postsListContainer || !archiveFiltersContainer) return;

    const searchBar = document.getElementById('searchBar');
    let currentFilter = 'all';
    let currentSearch = '';

    // Create filter buttons dynamically based on tags in search data
    function createFilterButtons() {
        // Get all unique tags from search data
        const allTags = new Set();
        searchData.forEach(post => {
            if (post.tags && Array.isArray(post.tags)) {
                post.tags.forEach(tag => {
                    if (tag) allTags.add(tag);
                });
            }
        });

        // Clear existing buttons
        archiveFiltersContainer.innerHTML = '';

        // Add "All" button
        const allButton = document.createElement('button');
        allButton.className = 'filter-btn active';
        allButton.dataset.tag = 'all';
        allButton.textContent = 'All';
        archiveFiltersContainer.appendChild(allButton);

        // Add buttons for each tag
        Array.from(allTags).sort().forEach(tag => {
            const button = document.createElement('button');
            button.className = 'filter-btn';
            button.dataset.tag = tag;
            button.textContent = tag;
            archiveFiltersContainer.appendChild(button);
        });

        // Add event listeners to all buttons
        const filterButtons = document.querySelectorAll('.filter-btn');
        filterButtons.forEach(button => {
            button.addEventListener('click', () => {
                filterButtons.forEach(btn => btn.classList.remove('active'));
                button.classList.add('active');
                currentFilter = button.dataset.tag;
                renderPosts();
            });
        });
    }

    // Use the global searchData instead of hardcoded posts
    function renderPosts() {
        const filteredPosts = searchData.filter(post => {
            const matchesFilter = currentFilter === 'all' || (post.tags && post.tags.includes(currentFilter));
            const title = post.title || '';
            const id = post.id || '';
            const searchMatch = (title.toLowerCase().includes(currentSearch.toLowerCase()) ||
                                id.toLowerCase().includes(currentSearch.toLowerCase()));
            return matchesFilter && searchMatch;
        });

        postsListContainer.innerHTML = '';

        // Update the counter
        if (archiveCounterElement) {
            archiveCounterElement.textContent = `Displaying ${filteredPosts.length} of ${searchData.length} entries`;
        }

        if (filteredPosts.length === 0) {
            postsListContainer.innerHTML = `<div class="no-results">No articles found matching your criteria.</div>`;
        } else {
            filteredPosts.forEach(post => {
                const postCard = document.createElement('div');
                postCard.className = 'post-card';

                // Get icon based on tags
                const icon = getIconForTags(post.tags);

                // Format the date if it exists
                let dateDisplay = '';
                if (post.date) {
                    dateDisplay = `<span>${post.date}</span>`;
                }

                // Create tags display
                let tagsDisplay = '';
                if (post.tags && Array.isArray(post.tags) && post.tags.length > 0) {
                    tagsDisplay = ' ' + post.tags.map(tag => `<span class="post-tag">${tag}</span>`).join(' ');
                }

                postCard.innerHTML = `
                    <div class="post-content">
                        <a href="${post.filepath}" class="post-title">
                            <i class="fas ${icon} post-icon"></i>
                            ${post.title || 'Untitled'}${tagsDisplay}
                        </a>
                        <p class="post-excerpt">${post.description || 'No description available.'}</p>
                        <div class="post-meta">
                            ${dateDisplay}
                            <span>ID: ${post.id}</span>
                        </div>
                    </div>`;
                postsListContainer.appendChild(postCard);
            });
        }
    }

    // Helper function to get an appropriate icon based on tags
    function getIconForTags(tags) {
        if (!tags || !Array.isArray(tags) || tags.length === 0) {
            return 'fa-file'; // Default icon
        }

        // Check for specific tags and return corresponding icons
        if (tags.includes('Mathematics') || tags.includes('math')) {
            return 'fa-infinity';
        }
        if (tags.includes('Technology') || tags.includes('code') || tags.includes('programming')) {
            return 'fa-robot';
        }
        if (tags.includes('Neuroscience') || tags.includes('brain')) {
            return 'fa-brain';
        }
        if (tags.includes('Physics')) {
            return 'fa-atom';
        }
        if (tags.includes('Computer Science') || tags.includes('cs')) {
            return 'fa-network-wired';
        }

        return 'fa-file'; // Default icon
    }

    searchBar.addEventListener('input', () => {
        currentSearch = searchBar.value;
        renderPosts();
    });

    // Initial setup - but only if we have data
    if (searchData.length > 0) {
        createFilterButtons();
        renderPosts();
    }
}

// =================================
// PAGE-SPECIFIC: COLLAGE/GALLERY PAGE
// =================================
function initializeGalleryPage() {
    const categoryButtons = document.querySelectorAll('.category-btn');
    if (categoryButtons.length === 0) return;

    categoryButtons.forEach(button => {
        button.addEventListener('click', () => {
            categoryButtons.forEach(btn => btn.classList.remove('active'));
            button.classList.add('active');
        });
    });
}

// =================================
// PAGE-SPECIFIC: ARTICLE PAGE (TOC)
// =================================
function initializeTableOfContents() {
    const tocContainer = document.getElementById('toc');
    if (!tocContainer) return;

    const tocToggle = document.getElementById('tocToggle');
    const tocList = document.getElementById('tocList');
    const headers = document.querySelectorAll('.post-content h2, .post-content h3');

    // Keep track of all generated IDs to ensure uniqueness
    const generatedIds = new Set();

    // helper function to generate URL-safe ID from text
    function generateIdFromText(text) {
        let id = text
            .toLowerCase()
            .trim()
            .replace(/[^\w\s-]/g, '') // remove non-alphanumeric characters except spaces and hyphens
            .replace(/[\s_-]+/g, '-') // replace spaces, underscores, and multiple hyphens with single hyphen
            .replace(/^-+|-+$/g, ''); // Remove leading and trailing hyphens

        // If ID starts with a number, prepend 'section-' to make it valid
        if (/^\d/.test(id)) {
            id = 'section-' + id;
        }

        // If ID is empty or only contains invalid characters, use a fallback
        if (!id) {
            id = 'header';
        }

        return id;
    }

    // generate Table of Contents
    headers.forEach((header, index) => {
        // Generate ID if it doesn't exist
        if (!header.id) {
            const headerText = header.textContent.trim();
            const baseId = generateIdFromText(headerText);

            // If no text content or empty after processing, use a fallback
            let finalId = baseId || `header-${index + 1}`;

            // Ensure unique ID by checking against all existing IDs and generated IDs
            let uniqueId = finalId;
            let counter = 1;

            // Check against existing IDs in the document and our generated IDs
            while (document.getElementById(uniqueId) || generatedIds.has(uniqueId)) {
                uniqueId = `${finalId}-${counter}`;
                counter++;
            }

            // Store the generated ID to prevent future conflicts
            generatedIds.add(uniqueId);
            header.id = uniqueId;
        } else {
            // If header already has an ID, add it to our tracking set
            generatedIds.add(header.id);
        }

        const link = document.createElement('a');
        const listItem = document.createElement('li');

        link.href = `#${header.id}`;
        link.textContent = header.textContent;

        if (header.tagName === 'H2') listItem.classList.add('toc-h2');
        if (header.tagName === 'H3') listItem.classList.add('toc-h3');

        link.addEventListener('click', function(e) {
            e.preventDefault();
            const targetElement = document.querySelector(`[id='${header.id}']`);
            if (targetElement) {
                window.scrollTo({ top: targetElement.offsetTop - 80, behavior: 'smooth' });
                if (window.innerWidth <= 1024) tocContainer.classList.remove('expanded');
            }
        });

        listItem.appendChild(link);
        tocList.appendChild(listItem);
    });

    // Toggle TOC on mobile
    if (tocToggle) {
        tocToggle.addEventListener('click', () => {
            tocContainer.classList.toggle('expanded');
        });
    }
}

// =================================
// PAGE-SPECIFIC: ORG-BLOCK STYLING
// =================================
function initializeOrgBlocks() {
    // Find all org-blocks with data-type attributes
    const orgBlocks = document.querySelectorAll('.org-block[data-type]');

    orgBlocks.forEach(block => {
        // Only process blocks that have data-type
        if (block.hasAttribute('data-type')) {
            const type = block.getAttribute('data-type');

            // Reset padding to default
            block.style.paddingTop = '';

            // Handle "dummy" as special case - don't display type, only title if it exists
            if (type === 'dummy') {
                // Remove any existing dynamic styling elements
                const existingHeader = block.querySelector('.org-block-header');
                if (existingHeader) existingHeader.remove();

                // If there's a data-title, create a simple title element
                if (block.hasAttribute('data-title')) {
                    // Create container for the header
                    const headerContainer = document.createElement('div');
                    headerContainer.className = 'org-block-header';
                    headerContainer.style.position = 'absolute';
                    headerContainer.style.top = '0';
                    headerContainer.style.left = '0';
                    headerContainer.style.right = '0';
                    headerContainer.style.zIndex = '1';

                    // Create the title element
                    const titleElement = document.createElement('div');
                    titleElement.className = 'org-block-title';
                    titleElement.textContent = block.getAttribute('data-title');
                    titleElement.style.padding = '0.3rem 0.8rem';
                    titleElement.style.fontSize = '0.8rem';
                    titleElement.style.fontWeight = '500';
                    titleElement.style.borderRadius = '4px 4px 0 0';
                    titleElement.style.whiteSpace = 'nowrap';
                    titleElement.style.overflow = 'hidden';
                    titleElement.style.textOverflow = 'ellipsis';

                    // Add title element to header
                    headerContainer.appendChild(titleElement);

                    // Add header to block
                    block.appendChild(headerContainer);

                    // Apply theme styles dynamically
                    applyOrgBlockTitleThemeStyles(block, titleElement);
                } else {
                    // No title, so no header needed - remove top padding
                    block.style.paddingTop = '1rem';
                }
                return; // Skip normal processing for dummy blocks
            }

            // Remove any existing dynamic styling elements
            const existingHeader = block.querySelector('.org-block-header');
            if (existingHeader) existingHeader.remove();

            // Set default top padding for non-dummy blocks
            block.style.paddingTop = '2.5rem';

            // Create container for the header
            const headerContainer = document.createElement('div');
            headerContainer.className = 'org-block-header';
            headerContainer.style.position = 'absolute';
            headerContainer.style.top = '0';
            headerContainer.style.left = '0';
            headerContainer.style.right = '0';
            headerContainer.style.display = 'flex';
            headerContainer.style.zIndex = '1';

            // Create the type element
            const typeElement = document.createElement('div');
            typeElement.className = 'org-block-type';
            typeElement.textContent = type;
            typeElement.style.padding = '0.3rem 0.8rem';
            typeElement.style.fontSize = '0.8rem';
            typeElement.style.fontWeight = '600';
            typeElement.style.textTransform = 'uppercase';
            typeElement.style.letterSpacing = '0.5px';
            typeElement.style.borderRadius = '4px 0 0 0';

            // Add type element to header
            headerContainer.appendChild(typeElement);

            // If there's a data-title, create the title element
            if (block.hasAttribute('data-title')) {
                const titleElement = document.createElement('div');
                titleElement.className = 'org-block-title';
                titleElement.textContent = block.getAttribute('data-title');
                titleElement.style.padding = '0.3rem 0.8rem';
                titleElement.style.fontSize = '0.8rem';
                titleElement.style.fontWeight = '500';
                titleElement.style.borderRadius = '0 4px 0 0';
                titleElement.style.flex = '1'; // Take remaining space
                titleElement.style.whiteSpace = 'nowrap';
                titleElement.style.overflow = 'hidden';
                titleElement.style.textOverflow = 'ellipsis';

                // Add title element to header
                headerContainer.appendChild(titleElement);
            } else {
                // If no title, make type element take full width with rounded corners
                typeElement.style.borderRadius = '4px 4px 0 0';
            }

            // Add header to block
            block.appendChild(headerContainer);

            // Apply theme styles dynamically
            applyOrgBlockThemeStyles(block, typeElement);
            if (block.hasAttribute('data-title')) {
                const titleElement = headerContainer.querySelector('.org-block-title');
                if (titleElement) {
                    applyOrgBlockTitleThemeStyles(block, titleElement);
                }
            }
        }
    });
}

// Apply theme styles to org-block type element
function applyOrgBlockThemeStyles(block, typeElement) {
    const body = document.body;
    const type = block.getAttribute('data-type');

    // Base theme styles
    if (body.classList.contains('dark-theme')) {
        typeElement.style.background = 'var(--dark-aqua)';
        typeElement.style.color = 'var(--dark-bg0)';
    } else {
        typeElement.style.background = 'var(--light-aqua)';
        typeElement.style.color = 'var(--light-bg0)';
    }

    // Type-specific colors (excluding "dummy")
    if (type !== 'dummy') {
        const typeColors = {
            'problem': body.classList.contains('dark-theme') ? 'var(--dark-yellow)' : 'var(--light-yellow)',
            'definition': body.classList.contains('dark-theme') ? 'var(--dark-green)' : 'var(--light-green)',
            'theorem': body.classList.contains('dark-theme') ? 'var(--dark-blue)' : 'var(--light-blue)',
            'proof': body.classList.contains('dark-theme') ? 'var(--dark-purple)' : 'var(--light-purple)'
        };

        // Apply specific color if defined, otherwise keep the default aqua color
        if (typeColors[type]) {
            typeElement.style.background = typeColors[type];
            // Update border color to match
            block.style.borderLeftColor = typeColors[type];
        } else {
            // For unknown block types, use the default aqua color for border
            if (body.classList.contains('dark-theme')) {
                block.style.borderLeftColor = 'var(--dark-aqua)';
            } else {
                block.style.borderLeftColor = 'var(--light-aqua)';
            }
        }
    } else {
        // For dummy blocks, use orange color
        if (body.classList.contains('dark-theme')) {
            block.style.borderLeftColor = 'var(--dark-orange)';
        } else {
            block.style.borderLeftColor = 'var(--light-orange)';
        }
    }
}

// Apply theme styles to org-block title element
function applyOrgBlockTitleThemeStyles(block, titleElement) {
    const body = document.body;
    const type = block.getAttribute('data-type');

    if (body.classList.contains('dark-theme')) {
        titleElement.style.background = 'var(--dark-bg1)';
        titleElement.style.color = 'var(--dark-fg1)';
        if (type !== 'dummy') {
            titleElement.style.borderBottom = '1px solid var(--dark-bg3)';
            titleElement.style.borderRight = '1px solid var(--dark-bg3)';
        }
    } else {
        titleElement.style.background = 'var(--light-bg1)';
        titleElement.style.color = 'var(--light-fg1)';
        if (type !== 'dummy') {
            titleElement.style.borderBottom = '1px solid var(--light-bg3)';
            titleElement.style.borderRight = '1px solid var(--light-bg3)';
        }
    }

    // For dummy blocks, we want a simpler style without borders
    if (type === 'dummy') {
        if (body.classList.contains('dark-theme')) {
            titleElement.style.background = 'var(--dark-bg2)';
            titleElement.style.color = 'var(--dark-fg2)';
            titleElement.style.borderBottom = 'none';
            titleElement.style.borderRight = 'none';
        } else {
            titleElement.style.background = 'var(--light-bg2)';
            titleElement.style.color = 'var(--light-fg2)';
            titleElement.style.borderBottom = 'none';
            titleElement.style.borderRight = 'none';
        }
    }
}

// =================================
// MAIN INITIALIZATION
// =================================
document.addEventListener('DOMContentLoaded', async () => {
    initializeThemeToggle();
    initializeGalleryPage();
    initializeTableOfContents();

    // Initialize search functionality
    await loadSearchData();

    // Initialize archive page after data is loaded
    initializeArchivePage();

    // Initialize org-block styling
    initializeOrgBlocks();
});