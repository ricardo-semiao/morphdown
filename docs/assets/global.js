"use strict";

// Modules:
// None for now


// Global variables:
window.RSSITE = {
    rem: 16, // In px
    media_width_lg: 920,
    media_width_md: 740
}


// Get the viewport width that accounts for scrollbar:
// https://www.smashingmagazine.com/2023/12/new-css-viewport-units-not-solve-classic-scrollbar-problem/
new ResizeObserver(() => {
  let vw = document.documentElement.clientWidth / 100;
  document.documentElement.style.setProperty("--vw", `${vw}px`);
}).observe(document.documentElement);


// Set current page:
document.addEventListener("DOMContentLoaded", () => {
    const navLinks = document.querySelectorAll("#rssite-header a");
    for (let i = 0; i < navLinks.length; i++) {
        const link = navLinks[i];
        const currentPath = window.location.pathname.replace(/(^\/|\/$)/g, '');
        const linkPath = link.pathname.replace(/(^\/|\/$)/g, '');
        const patternLinkPath = new RegExp("^" + linkPath.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + "(/|$)");

        if (patternLinkPath.test(currentPath)) {
            link.setAttribute("aria-current", "page");

            const dropContainer = link.closest(".nav-drop");
            if (dropContainer) {
                dropContainer.querySelector("button").setAttribute("aria-current", "page");
            }
        }
    }
});


// Handle .nav-drop aria-expanded:
document.addEventListener("DOMContentLoaded", () => {
    const navDrops = document.querySelectorAll("#rssite-header .nav-drop");

    navDrops.forEach(drop => {
        const button = drop.querySelector("button")
        let isHovered = false;
        let isFocused = false;
        
        drop.addEventListener("focusin", () => {
            isFocused = true;
            button.setAttribute("aria-expanded", "true");
        });
        drop.addEventListener("focusout", () => {
            setTimeout(() => {
                isFocused = false;
                if (!isHovered) {
                    button.setAttribute("aria-expanded", "false");
                }
            }, 50);
        });
        drop.addEventListener("mouseenter", () => {
            isHovered = true;
            button.setAttribute("aria-expanded", "true");
        });
        drop.addEventListener("mouseleave", () => {
            isHovered = false;
            if (!isFocused) {
                button.setAttribute("aria-expanded", "false");
            }
        });
    });
});


// Open bug report email:
async function openBugReportEmail() {
    const { UAParser } = await import('https://esm.sh/ua-parser-js');

    const email = "ricardo.semiao@outlook.com";
    const subject = encodeURIComponent("Bug report in ricardo-semiao.github.io");

    const { browser, os, device } = new UAParser().getResult();
    const browserOS = `${browser.name || 'Unknown'} ${browser.version || ''}, ${os.name || 'Unknown'} ${os.version || ''}`.trim();
    const deviceInfo = [device.type, device.vendor, device.model].filter(Boolean).join(' ') || 'Desktop / Generic';

    const bodyText = `Hello, I would like to report a bug in your website.\n
Description: [Brief explanation and screenshots of the bug].
Browser/OS (optional): ${browserOS}.
Device (optional): ${deviceInfo}.
Page: ${window.location.href}.\n
Greetings,`;

    const mailtoUrl = `mailto:${email}?subject=${subject}&body=${encodeURIComponent(bodyText)}`;
    window.location.href = mailtoUrl;
}

window.openBugReportEmail = openBugReportEmail;
