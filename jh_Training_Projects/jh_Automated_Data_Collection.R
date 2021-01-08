
# 수집데이터: 4400 rows / 148 pages / select and download
# 출처:기상자료개방포털
# 코트형태 : Full는 아니며, 활용 시 중요부분만 추출 및 기재

devtools::install_github("ChiHangChen/KeyboardSimulator")
install.packages("taskscheduleR")
library(KeyboardSimulator)
library(taskscheduleR)

# page_1
mouse.move(649,315) #Select all
mouse.click("left")
mouse.move(642,906) #Start_Download
mouse.click("left")
mouse.move(400,497, duration = 2) #_Category_Select
mouse.click("left")
mouse.move(487,581) #_Check
mouse.click("left")

mouse.move(953,1007, duration = 40) #_ x
mouse.click("left")

mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")


# page_2
mouse.move(482,904)
mouse.click("left")

# .
# .
# .
# .
# .
# .
# .

mouse.move(568,906, duration = 3) #_next_page
mouse.click("left")

mouse.move(962,1029, duration = 3) #_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")
mouse.move(962,1029) #Slect_down_arrow
mouse.click("left")

# ------------------------------------------------------------------------------from_11p

mouse.move(649,315, duration = 2) #Select all
mouse.click("left")

# .
# .
# .
# .
# .
# .
# .
