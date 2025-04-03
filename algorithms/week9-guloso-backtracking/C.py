def queen(board, row, coluna, d_primaria, d_secundaria, ans):
    if row == 8:
        ans[0] += 1
        return
    
    for col in range(8):
        if board[row][col] == '*' or coluna[col] or d_primaria[row - col + 8] or d_secundaria[row + col]:
            continue
        coluna[col] = d_primaria[row - col + 8] = d_secundaria[row + col] = True
        queen(board, row + 1, coluna, d_primaria, d_secundaria, ans)
        coluna[col] = d_primaria[row - col + 8] = d_secundaria[row + col] = False

board = [input() for _ in range(8)]
coluna, d_primaria, d_secundaria = [False] * 10, [False] * 20, [False] * 20
ans = [0]
queen(board, 0, coluna, d_primaria, d_secundaria, ans)
print(ans[0])