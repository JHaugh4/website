// When the user clicks on the button, toggle between hiding and showing the dropdown content
document.querySelector(".dropbtn").addEventListener("click", function() {
    console.log("clicked button");
    document.getElementById("dropdown-id").classList.toggle("show");
});

// Close the dropdown if the user clicks outside of it
window.onclick = function(event) {
    console.log("off button");
    if (!event.target.matches('.dropbtn')) {
        console.log("inside if");
        var dropdowns = document.getElementsByClassName("dropdown-content");
        for (var i = 0; i < dropdowns.length; i++) {
            var openDropdown = dropdowns[i];
            if (openDropdown.classList.contains('show')) {
                openDropdown.classList.remove('show');
            }
        }
    }
}
  