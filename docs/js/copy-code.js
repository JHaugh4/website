// document.addEventListener('DOMContentLoaded', function () {
//     document.querySelectorAll('.copy-button').forEach(function (button) {
//         button.addEventListener('click', function () {
//             var codeId = button.getAttribute('data-copy-target');
//             var codeElement = document.getElementById(codeId);
            
//             console.log(codeElement.textContent)
//             // Use the Clipboard API
//             navigator.clipboard.writeText(codeElement.textContent).then(function () {
//                 // On success: Provide feedback to the user
//                 button.textContent = 'Copied!';
//                 setTimeout(function () {
//                     button.textContent = '';
//                 }, 2000);
//             }).catch(function (err) {
//                 // On error: Log the error
//                 console.error('Could not copy text: ', err);
//             });
//         });
//     });
// });
// document.addEventListener('DOMContentLoaded', function () {
//     document.querySelectorAll('.copy-button').forEach(function (button) {
//         button.addEventListener('click', function () {
//             var codeId = button.getAttribute('data-copy-target');
//             var codeElement = document.getElementById(codeId);
//             var imgElement = button.querySelector('img');  // Select the img element
            
//             // Use the Clipboard API
//             navigator.clipboard.writeText(codeElement.textContent).then(function () {
//                 // On success: Provide feedback to the user
//                 // imgElement.src = '/images/copied-icon.png';  // Change to a "copied" icon
//                 imgElement.src = '';
//                 button.textContent = 'Copied!';
//                 setTimeout(function () {
//                     imgElement.src = '/images/copy-icon.png';  // Change back to the original icon
//                     console.log(imgElement.src);
//                     button.textContent = '';
//                 }, 2000);
//             }).catch(function (err) {
//                 // On error: Log the error
//                 console.error('Could not copy text: ', err);
//             });
//         });
//     });
// });

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