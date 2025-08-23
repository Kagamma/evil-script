import threading as thr
import time

def incrA():
  a=0
  for i in range(200000000):
    a+=1

def incrB():
  b=0
  for i in range(200000000):
    b+=1

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
print("Finished in : {}s".format(tEnd-tBegin))