document.addEventListener('DOMContentLoaded', () => {

    // =================================
    // COMMON: THEME TOGGLE FUNCTIONALITY
    // =================================
    const themeToggle = document.getElementById('themeToggle');
    const body = document.body;

    if (themeToggle) {
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

        function updateThemeIcon(theme) {
            if (theme === 'dark-theme') {
                themeIcon.className = 'fas fa-sun';
            } else {
                themeIcon.className = 'fas fa-moon';
            }
        }
    }


    // =================================
    // PAGE-SPECIFIC: ARCHIVE PAGE
    // =================================
    const postsListContainer = document.getElementById('postsList');
    if (postsListContainer) {
        const searchBar = document.getElementById('searchBar');
        const filterButtons = document.querySelectorAll('.filter-btn');
        let currentFilter = 'all';
        let currentSearch = '';

        const posts = [
            { id: 1, icon: 'fa-infinity', tag: 'Mathematics', title: 'title 1', excerpt: 'desc 1', date: 'June 15, 2023', readTime: '8 min read' },
            { id: 2, icon: 'fa-robot', tag: 'Technology', title: 'title 2', excerpt: 'desc 2', date: 'June 10, 2023', readTime: '12 min read' },
            { id: 3, icon: 'fa-brain', tag: 'Neuroscience', title: 'title 3', excerpt: 'desc 3', date: 'May 28, 2023', readTime: '10 min read' },
            { id: 4, icon: 'fa-atom', tag: 'Physics', title: 'title 4', excerpt: 'desc 4', date: 'May 15, 2023', readTime: '15 min read' },
            { id: 5, icon: 'fa-network-wired', tag: 'Computer Science', title: 'desc 5', excerpt: 'How mathematical graphs help us understand connections in the digital age.', date: 'April 30, 2023', readTime: '9 min read' },
            { id: 6, icon: 'fa-calculator', tag: 'Mathematics', title: 'title 6', excerpt: 'desc 6', date: 'April 12, 2023', readTime: '11 min read' },
        ];

        function renderPosts() {
            const filteredPosts = posts.filter(post => {
                const matchesFilter = currentFilter === 'all' || post.tag === currentFilter;
                const matchesSearch = post.title.toLowerCase().includes(currentSearch.toLowerCase());
                return matchesFilter && matchesSearch;
            });

            postsListContainer.innerHTML = '';

            if (filteredPosts.length === 0) {
                postsListContainer.innerHTML = `<div class="no-results">No articles found matching your criteria.</div>`;
            } else {
                filteredPosts.forEach(post => {
                    const postCard = document.createElement('div');
                    postCard.className = 'post-card';
                    postCard.innerHTML = `
                        <div class="post-icon"><i class="fas ${post.icon}"></i></div>
                        <div class="post-content">
                            <div class="post-header"><span class="post-tag">${post.tag}</span></div>
                            <a href="#" class="post-title">${post.title}</a>
                            <p class="post-excerpt">${post.excerpt}</p>
                            <div class="post-meta">
                                <span>${post.date}</span>
                                <span>${post.readTime}</span>
                            </div>
                        </div>`;
                    postsListContainer.appendChild(postCard);
                });
            }
        }

        filterButtons.forEach(button => {
            button.addEventListener('click', () => {
                filterButtons.forEach(btn => btn.classList.remove('active'));
                button.classList.add('active');
                currentFilter = button.dataset.tag;
                renderPosts();
            });
        });

        searchBar.addEventListener('input', () => {
            currentSearch = searchBar.value;
            renderPosts();
        });

        renderPosts(); // initial render
    }


    // =================================
    // PAGE-SPECIFIC: COLLAGE/GALLERY PAGE
    // =================================
    const categoryButtons = document.querySelectorAll('.category-btn');
    if (categoryButtons.length > 0) {
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
    const tocContainer = document.getElementById('toc');
    if (tocContainer) {
        const tocToggle = document.getElementById('tocToggle');
        const tocList = document.getElementById('tocList');
        const headers = document.querySelectorAll('.post-content h2, .post-content h3');

        // helper function to generate URL-safe ID from text
        function generateIdFromText(text) {
            return text
                .toLowerCase()
                .trim()
                .replace(/[^\w\s-]/g, '') // remove non-alphanumeric characters except spaces and hyphens
                .replace(/[\s_-]+/g, '-') // replace spaces, underscores, and multiple hyphens with single hyphen
                .replace(/^-+|-+$/g, ''); // Remove leading and trailing hyphens
        }

        // generate Table of Contents
        headers.forEach(header => {
            // Generate ID if it doesn't exist
            if (!header.id) {
                const headerText = header.textContent.trim();
                const generatedId = generateIdFromText(headerText);

                // Ensure unique ID by appending number if needed
                let uniqueId = generatedId;
                let counter = 1;
                while (document.getElementById(uniqueId)) {
                    uniqueId = `${generatedId}-${counter}`;
                    counter++;
                }

                header.id = uniqueId;
            }

            const link = document.createElement('a');
            const listItem = document.createElement('li');

            link.href = `#${header.id}`;
            link.textContent = header.textContent;

            if (header.tagName === 'H2') listItem.classList.add('toc-h2');
            if (header.tagName === 'H3') listItem.classList.add('toc-h3');

            link.addEventListener('click', function(e) {
                e.preventDefault();
                const targetElement = document.querySelector(this.getAttribute('href'));
                if (targetElement) {
                    window.scrollTo({ top: targetElement.offsetTop - 80, behavior: 'smooth' });
                    if (window.innerWidth <= 1024) tocContainer.classList.remove('expanded');
                }
            });

            listItem.appendChild(link);
            tocList.appendChild(listItem);
        });

        // Toggle TOC on mobile
        tocToggle.addEventListener('click', () => {
            tocContainer.classList.toggle('expanded');
        });
    }
});