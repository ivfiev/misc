import subprocess 
import traceback
import threading

timer = None

def log(msg):
	subprocess.check_output(['logger', '-t', __file__, msg])

def lower_volume():
	subprocess.check_output(['wpctl', 'set-volume', '@DEFAULT_SINK@', '0.35'])
	log('volume lowered')

def schedule():
	global timer
	if timer:
		timer.cancel()
	timer = threading.Timer(2.0, lower_volume)
	timer.start()

try:
	pwmon = subprocess.Popen(['pw-mon', '-p'], stdout=subprocess.PIPE, text=True, bufsize=1)
	while True:
		lines = []
		for line in pwmon.stdout:
			lines.append(line)
			if not line.strip():
				break
		if 'added:' in lines[0]:
			if any('A90 Pro' in line for line in lines):
				schedule()
except Exception as e:
	log(f'Fatal error: {str(e)}')
	log(f'Stacktrace: {''.join(traceback.format_exc())}')