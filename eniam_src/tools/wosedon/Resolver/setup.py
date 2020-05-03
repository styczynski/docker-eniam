#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

setup(name='Resolver',
	version='0.1.0',
	description='When some synset is only slightly better than some other, use ranking from another run of Wosedon.',
	author= "Michał Moczulski",
	author_email="michalmoczulski1a@gmail.com",
        packages=[
            'Resolver'
	],
	license='', 
	entry_points={
		'console_scripts': [
			'Resolver = Resolver.Resolver:main'
		]
	}
)
