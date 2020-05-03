#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

setup(name='CclExtractor',
	version='0.1.0',
	description='Extract synset graph from .ccl files.',
	author= "Michał Moczulski",
	author_email="michalmoczulski1a@gmail.com",
        packages=[
            'CclExtractor'
	],
	license='', 
	entry_points={
		'console_scripts': [
			'CclExtractor = CclExtractor.CclExtractor:main'
		]
	}
)
