import sys

args=sys.argv[1]
limit=int(args.split(" ")[1])

prev = 0
counter = 0
for line in sys.stdin.readlines():
    current = float(eval(line)[2])
    if prev > 0:
        print abs(current - prev)
    else:
        print 0
    prev = current
    counter = counter + 1
    if counter == limit:
        prev = 0
        counter = 0
    
