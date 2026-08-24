Shiny.addCustomMessageHandler('html2pdf', function(data) {
  var element = document.getElementById(data.id);
  
  // Forcem que tot el contingut sigui visible per la captura
  var originalOverflow = element.style.overflow;
  var originalHeight = element.style.height;
  element.style.overflow = 'visible';
  element.style.height = 'auto';

  // Fix per leaflet: invalida mida perquè es repinti sencer
  if (window.HTMLWidgets) {
    setTimeout(function() {
      window.dispatchEvent(new Event('resize'));
    }, 200);
  }

  var opt = {
    margin:       10,
    filename:     data.filename,
    image:        { type: 'jpeg', quality: 0.98 },
    html2canvas:  { 
      scale: 2, 
      useCORS: true, 
      allowTaint: true,
      scrollY: 0,
      scrollX: 0,
      windowWidth: document.documentElement.offsetWidth,
      windowHeight: document.documentElement.offsetHeight,
      width: element.scrollWidth,
      height: element.scrollHeight
    },
    jsPDF:        { unit: 'mm', format: 'a4', orientation: 'portrait' },
    pagebreak:    { mode: ['avoid-all', 'css', 'legacy'] }
  };

  html2pdf().set(opt).from(element).save().then(function(){
    element.style.overflow = originalOverflow;
    element.style.height = originalHeight;
  });
});