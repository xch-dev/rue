const isDarkMode = localStorage.getItem("dark-mode");

if (isDarkMode === "true") {
  document.documentElement.classList.add("dark");
}
