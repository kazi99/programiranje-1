
test_matrix = [[1,2,0], [2,4,5], [7,0,1]]

def max_cheese(cheese): 
    height = len(cheese)
    width = len(cheese[0])
    memo = [[0 for _ in range(width)] for _ in range (height)]

    for i in range(height - 1, -1, -1):
        for j in range(width - 1, -1, -1):

            dol = 0 if i == height - 1 else memo[i + 1][j]
            desno = 0 if j == width - 1 else memo[i][j + 1]

            memo[i][j] = cheese[i][j] + max(desno, dol)
            
    print(memo)
    return memo[0][0]

max_cheese(test_matrix)
