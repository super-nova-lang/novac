document.addEventListener('DOMContentLoaded', () => {
    const input = document.querySelector('#search');
    const themeSelect = document.querySelector('#theme-select');
    const sections = Array.from(document.querySelectorAll('section[data-signature]'));

    const themes = [
        'light',
        'dark',
        'monokai',
        'dracula',
        'solarized-light',
        'solarized-dark',
        'github-dark',
        'nord',
        'gruvbox'
    ];

    const applyTheme = (theme) => {
        // Remove all theme classes
        themes.forEach(t => document.body.classList.remove(t));

        // Add new theme class (light is default, no class needed)
        if (theme !== 'light') {
            document.body.classList.add(theme);
        }

        localStorage.setItem('docs-theme', theme);
        if (themeSelect) themeSelect.value = theme;
    };

    const saved = localStorage.getItem('docs-theme') || 'light';
    applyTheme(saved);

    const filter = () => {
        const term = input.value.toLowerCase().trim();
        sections.forEach(section => {
            const sig = (section.dataset.signature || '').toLowerCase();
            const doc = (section.dataset.doc || '').toLowerCase();
            const loc = section.querySelector('.loc').textContent.toLowerCase();
            const match = sig.includes(term) || doc.includes(term) || loc.includes(term);
            section.style.display = match ? '' : 'none';
        });
    };

    if (input) {
        input.addEventListener('input', filter);
    }

    if (themeSelect) {
        themeSelect.addEventListener('change', (e) => {
            applyTheme(e.target.value);
        });
    }
});
