#!/usr/bin/env python

from distutils.core import setup

LONG_DESCRIPTION = \
'''Generate my personal website'''


setup(
    name='website',
    version='0.1.0.0',
    author='Bernie Pope',
    author_email='bjpope@unimelb.edu.au',
    packages=['website'],
    package_dir={'website': 'website'},
    entry_points={
        'console_scripts': ['website = website.main:main',
            'csv2yaml = website.paperpile_csv_to_yaml:main']
    },
    url='https://github.com/bjpop/website',
    license='LICENSE',
    description=('Generate my personal website'),
    long_description=(LONG_DESCRIPTION),
    install_requires=["jinja2>=2.10.1", "pyyaml"],
)
