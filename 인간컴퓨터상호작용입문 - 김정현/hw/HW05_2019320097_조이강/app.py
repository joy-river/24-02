from flask import Flask, request, jsonify, render_template
from transformers import pipeline
from flask_cors import CORS
from datetime import datetime
import random

app = Flask(__name__)
CORS(app)

classifier = pipeline("text-classification", model="distilbert-base-multilingual-cased")

events = [
    {
        "id": 1,
        "name": "서울 뮤직 페스티벌",
        "date": "2024-01-15",
        "location": "서울",
        "transport": ["KTX", "버스"],
        "description": "국내외 인기 아티스트들이 참여하는 대규모 음악 축제입니다.",
    },
    {
        "id": 2,
        "name": "부산 아트 엑스포",
        "date": "2024-02-10",
        "location": "부산",
        "transport": ["KTX", "고속버스"],
        "description": "지역 예술가들의 다양한 작품을 전시하는 박람회입니다.",
    },
    {
        "id": 3,
        "name": "제주 푸드 축제",
        "date": "2024-03-20",
        "location": "제주도",
        "transport": ["비행기"],
        "description": "제주도의 신선한 재료로 만든 다양한 음식을 맛볼 수 있는 축제입니다.",
    },
    {
        "id": 4,
        "name": "대전 과학 박람회",
        "date": "2024-04-12",
        "location": "대전",
        "transport": ["KTX", "SRT"],
        "description": "과학 기술의 최신 트렌드를 경험할 수 있는 박람회입니다.",
    },
    {
        "id": 5,
        "name": "강릉 커피 축제",
        "date": "2024-05-05",
        "location": "강릉",
        "transport": ["KTX", "버스"],
        "description": "전국의 커피 애호가들을 위한 커피 시음 및 문화 행사입니다.",
    },
]

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/chat', methods=['POST'])
def chat():
    user_input = request.json.get('message')
    response = ""
    buttons = []

    # 모든 이벤트 보기
    if "모든" in user_input:
        response = "현재 예정된 모든 이벤트 목록은 다음과 같아요.<br>"
        buttons = [
            (f"{event['name']} ({event['date']}, {event['location']})<br>"
             f"<button onclick=\"fetchEventDetails({event['id']})\">상세보기</button> "
             f"<button class='styled-button' onclick=\"askTransport('{event['name']}')\">예약하기</button>")
            for event in events
        ]

    # 특정 위치 기반 추천
    elif "추천" in user_input:
        for event in events:
            if any(location in user_input for location in event["location"]):
                response = (f"이 행사는 어떠세요?<br>"
                            f"{event['name']} ({event['date']}, {event['location']})<br>"
                            "상세 정보는 아래 버튼을 눌러 확인해보세요!")
                buttons.append(f"<button onclick=\"fetchEventDetails({event['id']})\">상세보기</button> "
                               f"<button class='styled-button' onclick=\"askTransport('{event['name']}')\">예약하기</button>")
                break
        else:
            response = "해당 위치에서 추천할 행사가 없어요... 다시 검색해주세요!"

    # 교통 수단 선택 요청
    elif "예약" in user_input:
        matched_events = [
            event for event in events if event["name"] in user_input
        ]
        if matched_events:
            event = matched_events[0]
            response = (f"{event['name']} 예약을 진행합니다. 이동 수단을 선택해주세요.<br>"
                        "사용 가능한 옵션:")
            buttons = [
                f"<button onclick=\"reserveEvent('{event['name']}', '{transport}')\">{transport} 예약</button>"
                for transport in event["transport"]
            ]
        else:
            response = "예약할 행사를 찾을 수 없습니다. 정확한 이름을 입력해주세요."
            
    elif any(user_input in event["name"] or user_input in event["date"] for event in events):
        matched_events = [
            event for event in events if user_input in event["name"] or user_input in event["date"]
        ]
        if matched_events:
            response = "이 행사를 찾고 계신가요?<br>"
            buttons = [
                (f"{event['name']} ({event['date']}, {event['location']})<br>"
                 f"<button onclick=\"fetchEventDetails({event['id']})\">상세보기</button> "
                 f"<button class='styled-button' onclick=\"askTransport('{event['name']}')\">예약하기</button>")
                for event in matched_events
            ]
        else:
            response = "해당 이름이나 날짜에 맞는 행사를 찾을 수 없어요... 다시 검색해주세요!"
    
    # 기타 처리
    else:
        response = ("잘 모르겠어요. 다음 명령어를 사용해주세요!<br>"
                    "1. '추천 + [위치]'로 위치 기반 추천 받기<br>"
                    "2. '모든 추천'으로 전체 이벤트 보기<br>"
                    "3. 행사 이름 또는 날짜로 검색<br>"
                    "4. '예약'으로 예약 시작하기")

    return jsonify({"response": response, "buttons": buttons})

@app.route('/reserve', methods=['POST'])
def reserve():
    reservation_data = request.json
    event_name = reservation_data.get('event_name')
    transport = reservation_data.get('transport')

    event = next((e for e in events if e["name"] == event_name), None)
    if not event:
        return jsonify({"error": "해당 이름의 행사를 찾을 수 없습니다."}), 404

    reservation_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    reservation_id = random.randint(100000, 999999)

    reservation_details = {
        "confirmation_message": "예약이 완료되었습니다! 즐거운 여행 되세요!",
        "event_name": event["name"],
        "event_date": event["date"],
        "event_location": event["location"],
        "transport": transport,
        "reservation_time": reservation_time,
        "reservation_id": reservation_id,
    }
    return jsonify(reservation_details)

@app.route('/event/<int:event_id>', methods=['GET'])
def event_details(event_id):
    event = next((e for e in events if e["id"] == event_id), None)
    if not event:
        return jsonify({"error": "이벤트를 찾을 수 없습니다."}), 404
    
    return jsonify(event)

if __name__ == "__main__":
    app.run(debug=True)
