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
});