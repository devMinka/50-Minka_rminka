// pdfHandler.js (versión simplificada)
if (typeof html2pdf === "undefined") {
  var script = document.createElement("script");
  script.src = "https://cdnjs.cloudflare.com/ajax/libs/html2pdf.js/0.9.3/html2pdf.bundle.min.js";
  document.head.appendChild(script);
  script.onload = initHandler;
} else {
  initHandler();
}

function initHandler() {
  console.log("Handler registrado");

  if (typeof Shiny === "undefined") {
    console.error("Shiny no está cargado todavía.");
    return;
  }

  Shiny.addCustomMessageHandler("html2pdf", function (message) {
    var container = document.getElementById(message.id);
    console.log("Container:", container);
    if (!container) {
      console.error("No se encontró el contenedor con id:", message.id);
      return;
    }

    var opt = {
      margin:       0,
      filename:    message.filename,
      image:       { type: "jpeg", quality: 0.98 },
      html2canvas: { scale: 2, backgroundColor: "#ffffff" },
      jsPDF:       { unit: "mm", format: "a4", orientation: "portrait" },
      pagebreak:   { mode: "css" }
    };

    setTimeout(function () {
      html2pdf()
        .set(opt)
        .from(container)
        .save()
        .catch(function (err) {
          console.error("Error al generar el PDF:", err);
        });
    }, 500);
  });
}