
from pprint import pprint
from orgparse import load
from orgparse.node import OrgEnv

def do_clock(c):
  if c.has_end():
    return c.duration.total_seconds() / 60
  else:
    return 0

myenv = OrgEnv(
  todos = ['TODO', 'NEXT', 'STARTED'],
  dones = ['DONE', 'CANCELED'],
  filename = '/home/fnaufel/Documents/OrgFiles/development.org'
)

root = load(
  '/home/fnaufel/Documents/OrgFiles/development.org',
  env = myenv
)

current_level = 0
current_heading = ''

for node in root[1:]:
  clocks = node.clock
  if len(clocks) > 0:
    pprint(node.get_heading())
    pprint([do_clock(c) for c in clocks])
    print()

  # prev_level = current_level
  # prev_heading = current_heading
  # current_level = node.level
  # current_heading = node.heading
  # dif = current_level - prev_level
  # if dif > 2:
  #   print(dif, prev_heading, '-->', current_heading)
