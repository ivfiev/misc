
seaxch.pyz: *.py
	pip list --not-required | tail -n +3 | grep -v 'shiv' | awk '{print $$1}' > requirements-shiv.txt
	shiv -e main:main -o seaxch.pyz -r requirements-shiv.txt --site-packages .
