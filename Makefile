
build-docker:
	docker build -t styczynski/eniam:1.0 .

publish:
	rm -rfd dist
	rm -rfd eniam.egg-info
	python3 setup.py sdist bdist_wheel && python3 -m twine upload dist/*