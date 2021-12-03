app = Application.currentApplication()
app.includeStandardAdditions = true
const currentTab = Application('Safari').windows[0].currentTab
const url = currentTab.url()
const name = currentTab.name()
app.openLocation(`org-protocol://roam-ref?template=r&ref=${encodeURIComponent(url)}&title=${encodeURIComponent(name)}`)
