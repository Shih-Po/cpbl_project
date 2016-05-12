# cpbl_project
這個 repository 做到的事情：
 1. 「文字轉播紀錄」轉換為「結構化表格」（追蹤壘包出局數）
 2. 計算器：得分矩陣
 3. 計算器：跑壘積極度計算器


### 1. 結構化表格（追蹤壘包出局數）
#### main_fullseason.R 
記錄計算得分矩陣與跑壘積極度的資料，追蹤每一筆 log_row 中的壘包出局數，以得分矩陣的情境區分開。

- input: 育修完成換行的 log 檔
- output: 以每次進攻情況作為區分的 off_db data.frame

off_db 欄位解析：
 1. num_logfile, num_logrow, inning: 比賽場次、log 記錄行數、比賽局數
 2. rem_type, base1, base2, base3： 當前情況的得分矩陣情境、壘包上球員名稱
 3. player: 此次情況要行動的球員（這一位球員的行動結果會暫存在 dummy_list 中，存到下一次情況時的欄位中）


### 2. 得分矩陣計算
#### tools_rem/rem_operator_function
 - input: 手動清理過後的結構化表格（注意這裡使用的結構化表格是 judy 整理過後，經由組員們人工校正的完整版）
 - output: 聯盟得分矩陣、球隊得分矩陣、球員得分矩陣

### 3. 跑壘積極度計算
#### tools_ebr/
待補 
