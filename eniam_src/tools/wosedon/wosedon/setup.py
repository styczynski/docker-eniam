#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

setup(name='wosedon',
	version='0.1.0',
	description='Word Sense Disambiguation',
	author= "Paweł Kędzia, Marlena Orlińska",
	author_email="Pawel.Kedzia@pwr.edu.pl, Marlena.Orlinska@pwr.edu.pl",
        packages=[
            'wosedon',
            'wosedon.algorithms', 'wosedon.context',
            'wosedon.gbuilders', 'wosedon.mergers', 
            'wosedon.reranks', 'wosedon.utils',
            'wosedon.ranking', 'wosedon.lesk_functions',
            'wosedon.lesk_filters'
	],
	license='LGPL', 
	entry_points={
		'console_scripts': [
			'wosedon = wosedon.run_wosedon:main',
			'posedon = wosedon.run_wosedon_pos:main',
			'wosedon-eval = wosedon.evaluation:main',
			'wosedon-gen-prec = wosedon.prec_general:main'
		]
	}
)
