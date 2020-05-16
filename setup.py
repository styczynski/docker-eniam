from setuptools import setup
import setuptools

# read the contents of your README file
from os import path
this_directory = path.abspath(path.dirname(__file__))
with open(path.join(this_directory, 'README.md')) as f:
    long_description = f.read()

setup(
    name='eniam',
    version='1.0',
    license='MIT',
    author='styczynski',
    author_email='piotr@styczynski.in',
    url='http://styczynski.in',
    long_description=long_description,
    long_description_content_type='text/markdown',
    packages=setuptools.find_packages(),
    scripts=['bin/eniam-cli'],
    include_package_data=True,
    package_data={},
    install_requires=[
        'opencv-python==4.2.0.32',
        'camelot-py==0.7.2',
        'wand==0.5.8',
        'beautifulsoup4==4.9.0'
    ],
    description="Cross-platform python wrapper around ENIAM (http://eniam.nlp.ipipan.waw.pl/)",
)