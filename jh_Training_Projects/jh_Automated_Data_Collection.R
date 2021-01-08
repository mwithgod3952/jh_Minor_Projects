# 수집데이터: 4400 rows / 148 pages / select and download
# 출처:기상자료개방포털_종관기상관측(https://data.kma.go.kr/data/grnd/selectAsosRltmList.do?pgmNo=36&tabNo=1)
# 수집데이터 목록
#     1_ 도시 
#           :17개도시 > 서울, 부산, 대구, 인청,광주,대전,울산,경기도,강원도,충청북도,충청남도,전라북도,전라남도,경상남도,제주도,세종특별자치시
#     2_ 기간
#           :1970년도부터 2021년도_ 약 50년간 기상관측정보
#     3_ 자료형태
#           : 시간단위 관측정보

devtools::install_github("ChiHangChen/KeyboardSimulator")
install.packages("taskscheduleR")
library(KeyboardSimulator)
library(taskscheduleR)

# 첫페이지 접근
mouse.move(649,315) #카테고리 내 모두 선택
mouse.click("left")
mouse.move(642,906) #다운로드 시작
mouse.click("left")
mouse.move(400,497, duration = 2) #자료사용 목적 선택 필요
mouse.click("left")
mouse.move(487,581) #_Check
mouse.click("left")

mouse.move(953,1007, duration = 40) #다운로드확인
mouse.click("left")

mouse.move(962,1029) #스크롤다운
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")

# 두번째페이지 접근
mouse.move(482,904)
mouse.click("left")

# 이하 같은작업 반복
# .

mouse.move(568,906, duration = 3) #Duration 고려 : 10개 그룹 단위로 새로고침 필요
mouse.click("left")

mouse.move(962,1029, duration = 3) #선택범위 이동 : 스크롤다운
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")
mouse.move(962,1029) 
mouse.click("left")

# ------------------------------------------------------------------------------from_11p

mouse.move(649,315, duration = 2) #다운로드 시작
mouse.click("left")

# 이하 같은작업 반복
# .
