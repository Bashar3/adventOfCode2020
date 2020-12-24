from functools import reduce

with open("input.txt", "r") as groups:
    groups = groups.read().split("\n\n")
answer = []
for g in groups:
    answer2 = set(g.split()[0])
    for person in g.split():
        answer2 = answer2.intersection(set(person))
    answer.append(len(answer2))
answer = reduce(lambda x, y: x + y, answer)
print(answer)
