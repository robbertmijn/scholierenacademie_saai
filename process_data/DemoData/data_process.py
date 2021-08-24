import json
import csv

def convert_results_1sec():
  results = []
  filename = '1sec'

  with open(filename + '.csv', mode='w') as csv_file:
    fieldnames = ['subject_nr', 'response_time_keyboard_resp']
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    with open(filename + '.txt') as f:
      for idx, line in enumerate(f):
        result = json.loads(line[:-2])
        if 'browser' in result:
          # skip weird metadata line
          continue
        if 'response_time_keyboard_resp' not in result:
          # skip this line if it doesn't have the correct format
          continue
        data = {'subject_nr': result['studyResultId'],
        'response_time_keyboard_resp': result['response_time_keyboard_resp']}
        writer.writerow(data)

def convert_results_tur():
  results = []
  filename = 'tur'

  with open(filename + '.csv', mode='w') as csv_file:
    fieldnames = ['subject_nr', 'response_time_reproduction', 'punish', 'total_points']
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    with open(filename + '.txt') as f:
      for idx, line in enumerate(f):
        result = json.loads(line[:-1])

        for row in result['data']:
          data = {'subject_nr': str(idx), 
                  'response_time_reproduction': row['response_time_reproduction'],
                  'punish': row['punish'],
                  'total_points': row['total_points']}
          writer.writerow(data)

def convert_results_context():
  results = []
  filename = 'context'

  with open(filename + '.csv', mode='w') as csv_file:
    fieldnames = ['subject_nr', 'response_time_reproduction', 'context', 'exp_dur']
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    with open(filename + '.txt') as f:
      for idx, line in enumerate(f):
        result = json.loads(line[:-1])

        for row in result['data']:
          data = {'subject_nr': str(idx), 
                  'response_time_reproduction': row['response_time_reproduction'],
                  'context': row['context'],
                  'exp_dur': row['exp_dur']}
          writer.writerow(data)

convert_results_1sec()
convert_results_tur()
convert_results_context()