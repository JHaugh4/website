document.addEventListener('DOMContentLoaded', function () {
    document.querySelectorAll('.copy-button').forEach(function (button) {
        button.addEventListener('click', function () {
            var codeId = button.getAttribute('data-copy-target');
            var codeElement = document.getElementById(codeId);
            var copyIcon = button.querySelector('#copyIconId');  
            var copyCodeElement = button.querySelector('#copyCodeId');
            var copiedIcon = button.querySelector('#copiedIconId');
            var copiedElement = button.querySelector('#copiedId'); 
            
            // Use the Clipboard API
            navigator.clipboard.writeText(codeElement.textContent).then(function () {
                // On success: Provide feedback to the user
                copyIcon.style.display = 'none';
                copyCodeElement.style.display = 'none';
                copiedIcon.style.display = 'inline';
                copiedElement.style.display = 'inline'; 
                setTimeout(function () {
                    copyIcon.style.display = 'inline';
                    copyCodeElement.style.display = 'inline';
                    copiedIcon.style.display = 'none';
                    copiedElement.style.display = 'none';
                }, 2000);
            }).catch(function (err) {
                // On error: Log the error
                console.error('Could not copy text: ', err);
            });
        });
    });
});