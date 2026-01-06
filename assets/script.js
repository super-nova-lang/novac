document.addEventListener('DOMContentLoaded', () => {
    const input = document.querySelector('#search');
    const toggle = document.querySelector('#toggle-theme');
    const sections = Array.from(document.querySelectorAll('section[data-signature]'));

    const applyTheme = (mode) => {
        document.body.classList.toggle('dark', mode === 'dark');
        localStorage.setItem('docs-theme', mode);
        if (toggle) toggle.textContent = mode === 'dark' ? 'Light mode' : 'Dark mode';
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

    input.addEventListener('input', filter);
    toggle.addEventListener('click', () => {
        const isDark = document.body.classList.contains('dark');
        applyTheme(isDark ? 'light' : 'dark');
    });
});
