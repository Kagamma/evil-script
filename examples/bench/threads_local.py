import threading as thr
import time

ga = 0
gb = 0

def incrA():
  a = 0
  for i in range(200000001):
    a += 1  
  global ga
  ga = a

def incrB():
  b = 0
  for i in range(200000001):
    b += 1
  global gb
  gb = b

def runThreads():
  thA=thr.Thread(target=incrA)
  thB=thr.Thread(target=incrB)
  thA.start()
  thB.start()
  thA.join()
  thB.join()

tBegin=time.time()

runThreads()

tEnd=time.time()

print("a = ", ga, ", b = ", gb)
print("Finished in : {}s".format(tEnd-tBegin))