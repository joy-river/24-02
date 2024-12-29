function toggleRecommend() {
  const recommendSection = document.getElementById("recommendSection");
  const showButton = document.getElementById("showRecommendButton");
  const isCollapsed = recommendSection.classList.toggle("collapsed");

  if (isCollapsed) {
    showButton.classList.add("visible");
  } else {
    showButton.classList.remove("visible");
  }
}

function showRecommend() {
  const recommendSection = document.getElementById("recommendSection");
  const showButton = document.getElementById("showRecommendButton");

  recommendSection.classList.remove("collapsed");
  showButton.classList.remove("visible");
}

function sendMessage() {
  const userInput = document.getElementById("userInput").value;

  if (userInput.trim() === "") return;

  const chatbot = document.getElementById("chatbot");
  const userMessage = document.createElement("div");
  userMessage.classList.add("chat-message", "user");
  userMessage.innerHTML = `<div class="message user">${userInput}</div>`;
  chatbot.appendChild(userMessage);

  fetch("/chat", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ message: userInput }),
  })
    .then((response) => response.json())
    .then((data) => {
      const botMessage = document.createElement("div");
      botMessage.classList.add("chat-message", "bot");
      botMessage.innerHTML = `<div class="message bot">${data.response}</div>`;
      chatbot.appendChild(botMessage);

      if (data.buttons.length > 0) {
        data.buttons.forEach((buttonHTML) => {
          const buttonElement = document.createElement("div");
          buttonElement.innerHTML = buttonHTML;
          buttonElement.querySelector("button").classList.add("styled-button"); // 버튼에 스타일 클래스 추가
          chatbot.appendChild(buttonElement);
        });
      }

      chatbot.scrollTop = chatbot.scrollHeight;
    })
    .catch(() => {
      const errorMessage = document.createElement("div");
      errorMessage.classList.add("chat-message", "bot");
      errorMessage.innerHTML = `<div class="message bot">알 수 없는 명령입니다. 아래 매뉴얼을 참조하세요:<br>1. "추천" + [위치]로 이벤트 추천 받기<br>2. "모든"으로 모든 이벤트 보기</div>`;
      chatbot.appendChild(errorMessage);

      chatbot.scrollTop = chatbot.scrollHeight;
    });

  document.getElementById("userInput").value = "";
}

function askTransport(eventName) {
  const chatbot = document.getElementById("chatbot");
  const botMessage = document.createElement("div");
  botMessage.classList.add("chat-message", "bot");
  botMessage.innerHTML = `<div class="message bot">어떤 교통 수단으로 예약할까요?</div>`;
  chatbot.appendChild(botMessage);

  fetch("/chat", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ message: `${eventName} 예약` }),
  })
    .then((response) => response.json())
    .then((data) => {
      data.buttons.forEach((buttonHTML) => {
        const buttonElement = document.createElement("div");
        buttonElement.innerHTML = buttonHTML;
        buttonElement.querySelector("button").classList.add("styled-button"); // 스타일 클래스 추가
        chatbot.appendChild(buttonElement);
      });
      chatbot.scrollTop = chatbot.scrollHeight;
    });
}

function reserveEvent(eventName, transport) {
  fetch("/reserve", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ event_name: eventName, transport: transport }),
  })
    .then((response) => response.json())
    .then((data) => {
      if (data.error) {
        alert(data.error);
      } else {
        const stylePath = "/static/css/style.css";
        const confirmation = `
                <!DOCTYPE html>
                <html>
                <head>
                    <link rel="stylesheet" href="${stylePath}">
                    <title>예약 확인서</title>
                    <script>
                        function downloadConfirmation() {
                            const content = \`
                            예약 확인서
                            ---------------------
                            행사명: ${data.event_name}
                            예약 번호: ${data.reservation_id}
                            예약 일시: ${data.reservation_time}
                            날짜: ${data.event_date}
                            위치: ${data.event_location}
                            교통수단: ${data.transport}
                            ---------------------
                            \`;
                            const blob = new Blob([content], { type: "text/plain" });
                            const link = document.createElement("a");
                            link.href = URL.createObjectURL(blob);
                            link.download = "${data.event_name}_예약확인서.txt";
                            link.click();
                        }
                    </script>
                </head>
                <body>
                    <header>
                        <h1>${data.event_name} 예약 확인서</h1>
                    </header>
                    <main>
                        <section class="event-details">
                            <p><strong>예약 번호:</strong> ${data.reservation_id}</p>
                            <p><strong>예약 일시:</strong> ${data.reservation_time}</p>
                            <p><strong>날짜:</strong> ${data.event_date}</p>
                            <p><strong>위치:</strong> ${data.event_location}</p>
                            <p><strong>교통수단:</strong> ${data.transport}</p>
                            <p>${data.confirmation_message}</p>
                            <button onclick="downloadConfirmation()">다운로드</button>
                        </section>
                    </main>
                </body>
                </html>
            `;
        const newWindow = window.open("", "_blank");
        newWindow.document.write(confirmation);
        newWindow.document.close();
      }
    });
}

const style = document.createElement("style");
style.textContent = `
    .styled-button {
        padding: 10px 15px;
        background-color: #0056b3;
        color: white;
        border: none;
        border-radius: 5px;
        font-size: 1em;
        cursor: pointer;
        transition: background-color 0.3s ease;
    }
    .styled-button:hover {
        background-color: #004494;
    }
    .styled-button:active {
        background-color: #003377;
    }
`;
document.head.appendChild(style);

function fetchEventDetails(eventId) {
  fetch(`/event/${eventId}`)
    .then((response) => response.json())
    .then((event) => {
      const stylePath = "/static/css/style.css";
      const template = `
                <!DOCTYPE html>
                <html>
                <head>
                    <link
                      rel="stylesheet"
                      href="${stylePath}"
                    />
                    <title>${event.name}</title>
                </head>
                <body>
                    <header>
                        <h1>${event.name}</h1>
                    </header>
                    <main>
                        <section class="event-details">
                            <p><strong>날짜:</strong> ${event.date}</p>
                            <p><strong>위치:</strong> ${event.location}</p>
                            <p><strong>설명:</strong> ${event.description}</p>
                            <p><strong>이동수단:</strong> ${event.transport.join(
                              ", "
                            )}</p>
                        </section>
                    </main>
                </body>
                </html>
            `;
      const newWindow = window.open("", "_blank");
      newWindow.document.write(template);
      newWindow.document.close();
    });
}
