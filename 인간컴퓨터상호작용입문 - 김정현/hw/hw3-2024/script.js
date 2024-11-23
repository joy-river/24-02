const reservationTable = document
  .getElementById("reservationTable")
  .querySelector("tbody");
const reservationFormEl = document.getElementById("reservationFormEl");
const downloadCsvBtn = document.getElementById("downloadCsvBtn");
const togglePrivacyBtn = document.getElementById("togglePrivacyBtn");
const createReservationBtn = document.getElementById("createReservationBtn");
const reservationForm = document.getElementById("reservationForm");
const fileNameInput = document.getElementById("fileName");
const backToMenuBtn = document.getElementById("backToMenuBtn");
const csvFileInput = document.getElementById("csvFileInput");
const loadCsvBtn = document.getElementById("loadCsvBtn");
const filterText = document.getElementById("filterText");
const applyFilterBtn = document.getElementById("applyFilterBtn");
const resetFilterBtn = document.getElementById("resetFilterBtn");
const detailsContent = document.getElementById("detailsContent");
const reservationDetails = document.getElementById("reservationDetails");
const backToReservationsBtn = document.getElementById("backToReservationsBtn");

let reservations = [];
let privacyMode = true;
let filteredReservations = null;

window.addEventListener("DOMContentLoaded", () => {
  loadCSV("test.csv");
});

function loadCSV(filePath) {
  fetch(filePath)
    .then((response) => {
      if (!response.ok) {
        throw new Error(`Failed to fetch ${filePath}`);
      }
      return response.text();
    })
    .then((csvText) => {
      reservations = parseCSV(csvText);
      renderTable();
    })
    .catch((error) => console.error("Error loading CSV:", error));
}

loadCsvBtn.addEventListener("click", () => {
  const file = csvFileInput.files[0];
  if (file) {
    const reader = new FileReader();
    reader.onload = (event) => {
      reservations = parseCSV(event.target.result);
      renderTable();
    };
    reader.readAsText(file);
  } else {
    alert("Please select a CSV file.");
  }
});

function parseCSV(csvText) {
  const rows = csvText.split("\n").filter((row) => row.trim());
  return rows.map((row) => {
    const [date, time, trainNo, weather, from, destination, privacy] = row
      .split(",")
      .map((col) => col.trim());
    return { date, time, trainNo, weather, from, destination, privacy };
  });
}
function renderTable(data = reservations) {
  reservationTable.innerHTML = "";
  data.forEach((reservation, index) => {
    const row = reservationTable.insertRow();

    row.insertCell(0).textContent = reservation.date;
    row.insertCell(1).textContent = reservation.time;
    row.insertCell(2).textContent = reservation.trainNo;
    row.insertCell(3).textContent = reservation.weather;
    row.insertCell(4).textContent = reservation.from;
    row.insertCell(5).textContent = reservation.destination;
    row.insertCell(6).textContent = privacyMode ? "*****" : reservation.privacy;

    const actionsCell = row.insertCell(7);
    const detailsBtn = document.createElement("button");
    const editBtn = document.createElement("button");
    const deleteBtn = document.createElement("button");

    detailsBtn.textContent = "Details";
    editBtn.textContent = "Edit";
    deleteBtn.textContent = "Delete";

    detailsBtn.addEventListener("click", () => showDetails(reservation));
    editBtn.addEventListener("click", () => editReservation(index));
    deleteBtn.addEventListener("click", () => deleteReservation(index));

    actionsCell.appendChild(detailsBtn);
    actionsCell.appendChild(editBtn);
    actionsCell.appendChild(deleteBtn);
  });
}

function showDetails(reservation) {
  reservationDetails.style.display = "block";
  detailsContent.innerHTML = `
        <strong>Date:</strong> ${reservation.date}<br>
        <strong>Time:</strong> ${reservation.time}<br>
        <strong>Train No:</strong> ${reservation.trainNo}<br>
        <strong>Weather:</strong> ${reservation.weather}<br>
        <strong>From:</strong> ${reservation.from}<br>
        <strong>Destination:</strong> ${reservation.destination}<br>
        <strong>Privacy:</strong> ${privacyMode ? "*****" : reservation.privacy}
    `;
}

backToReservationsBtn.addEventListener("click", () => {
  reservationDetails.style.display = "none";
});

applyFilterBtn.addEventListener("click", () => {
  const keyword = filterText.value.trim().toLowerCase();
  if (keyword) {
    filteredReservations = reservations.filter((reservation) =>
      reservation.destination.toLowerCase().includes(keyword)
    );
    renderTable(filteredReservations);
  } else {
    alert("Please enter a valid keyword to filter.");
  }
});

resetFilterBtn.addEventListener("click", () => {
  filteredReservations = null;
  filterText.value = "";
  renderTable();
});

togglePrivacyBtn.addEventListener("click", () => {
  privacyMode = !privacyMode;
  renderTable(filteredReservations || reservations);
});

function deleteReservation(index) {
  reservations.splice(index, 1);
  renderTable(filteredReservations || reservations);
}

function editReservation(index) {
  const reservation = reservations[index];
  const newDestination = prompt(
    "Enter new destination:",
    reservation.destination
  );
  if (newDestination) {
    reservation.destination = newDestination;
  }
  renderTable(filteredReservations || reservations);
}

reservationFormEl.addEventListener("submit", (event) => {
  event.preventDefault();

  const date = document.getElementById("date").value;
  const time = document.getElementById("time").value;
  const trainNo = document.getElementById("trainNo").value;
  const weather = document.getElementById("weather").value;
  const from = document.getElementById("from").value;
  const destination = document.getElementById("destination").value;
  const privacy = document.getElementById("privacy").value;

  reservations.push({
    date,
    time,
    trainNo,
    weather,
    from,
    destination,
    privacy,
  });
  renderTable(filteredReservations || reservations);

  reservationForm.style.display = "none";
  reservationFormEl.reset();
});

document.querySelectorAll(".sortBtn").forEach((button) => {
  button.addEventListener("click", () => {
    const column = button.getAttribute("data-column");
    reservations.sort((a, b) => a[column].localeCompare(b[column]));
    renderTable(filteredReservations || reservations);
  });
});

createReservationBtn.addEventListener("click", () => {
  reservationForm.style.display = "block";
});

backToMenuBtn.addEventListener("click", () => {
  reservationForm.style.display = "none";
});

downloadCsvBtn.addEventListener("click", () => {
  const fileName = fileNameInput.value || "reservations.csv";
  const csvContent =
    "Date,Time,Train No,Weather,From,Destination,Privacy\n" +
    reservations
      .map(
        (r) =>
          `${r.date},${r.time},${r.trainNo},${r.weather},${r.from},${r.destination},${r.privacy}`
      )
      .join("\n");

  const blob = new Blob([csvContent], { type: "text/csv" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = fileName;
  link.click();
});
