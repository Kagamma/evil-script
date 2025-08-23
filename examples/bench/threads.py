import threading as thr
import time

class TestThreads:
  def __init__(self):
    self.a=0
    self.b=0

  def incrA(self):
    a=0
    for i in range(200000000):
      a+=1
    self.a=a

  def incrB(self):
    b=0
    for i in range(200000000):
      b+=1
    self.b=b

  def runThreads(self):
    thA=thr.Thread(target=self.incrA)
    thB=thr.Thread(target=self.incrB)
    thA.start()
    thB.start()
    thA.join()
    thB.join()

tBegin=time.time()

t=TestThreads()
t.runThreads()

tEnd=time.time()
print("Finished in : {}s".format(tEnd-tBegin))