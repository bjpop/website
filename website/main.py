'''
Module      : Main 
Description : The main entry point for the program.
Copyright   : (c) Bernie Pope, 2017 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Portability : POSIX

Generate my website
'''

from argparse import ArgumentParser
import sys
import logging
import pkg_resources
import os
from jinja2 import Environment, FileSystemLoader
import yaml


EXIT_FILE_IO_ERROR = 1
EXIT_COMMAND_LINE_ERROR = 2
DEFAULT_VERBOSE = False
DEFAULT_TEMPLATES_DIR = 'templates' 
DEFAULT_OUTPUT_DIR = 'docs' 
PROGRAM_NAME = "website"
# Number of latest publications to show on index.html
MAX_LATEST_PUBLICATIONS = 3
# Number of latest presentations to show on index.html
MAX_LATEST_PRESENTATIONS = 3


try:
    PROGRAM_VERSION = pkg_resources.require(PROGRAM_NAME)[0].version
except pkg_resources.DistributionNotFound:
    PROGRAM_VERSION = "undefined_version"


def exit_with_error(message, exit_status):
    '''Print an error message to stderr, prefixed by the program name and 'ERROR'.
    Then exit program with supplied exit status.

    Arguments:
        message: an error message as a string.
        exit_status: a positive integer representing the exit status of the
            program.
    '''
    logging.error(message)
    print("{} ERROR: {}, exiting".format(PROGRAM_NAME, message), file=sys.stderr)
    sys.exit(exit_status)


def parse_args():
    '''Parse command line arguments.
    Returns Options object with command line argument values as attributes.
    Will exit the program on a command line error.
    '''
    parser = ArgumentParser(description='Generate my website')
    parser.add_argument('--version',
        action='version',
        version='%(prog)s ' + PROGRAM_VERSION)
    parser.add_argument('--log',
        metavar='LOG_FILE',
        type=str,
        help='record program progress in LOG_FILE')
    parser.add_argument('--templates',
        metavar='TEMPLATES_DIR',
        type=str,
        help='path to the templates directory',
        default=DEFAULT_TEMPLATES_DIR)
    parser.add_argument('--outdir',
        metavar='OUTPUT_DIR',
        type=str,
        help='path to write output HTML files',
        default=DEFAULT_OUTPUT_DIR)
    return parser.parse_args()


def init_logging(log_filename):
    '''If the log_filename is defined, then
    initialise the logging facility, and write log statement
    indicating the program has started, and also write out the
    command line from sys.argv

    Arguments:
        log_filename: either None, if logging is not required, or the
            string name of the log file to write to
    Result:
        None
    '''
    if log_filename is not None:
        logging.basicConfig(filename=log_filename,
            level=logging.DEBUG,
            filemode='w',
            format='%(asctime)s %(levelname)s - %(message)s',
            datefmt='%m-%d-%Y %H:%M:%S')
        logging.info('program started')
        logging.info('command line: {0}'.format(' '.join(sys.argv)))


def init_jinja(options):
    return Environment(
        autoescape=False,
        loader=FileSystemLoader(options.templates),
        trim_blocks=True)

 
def latest_publications(publication_list):
    # we assume the publication list is kept in date sorted order in the YAML file
    # so we don't need to sort it here
    return publication_list[:MAX_LATEST_PUBLICATIONS]

def latest_presentations(presentation_list):
    # we assume the presentation list is kept in date sorted order in the YAML file
    # so we don't need to sort it here
    return presentation_list[:MAX_LATEST_PRESENTATIONS]


def render_pages(options, jinja_env):
    index_template = Template("index.html")
    index_template.add_content("contents", options.templates, "index.yaml")
    index_template.add_content("publications", options.templates, "publications.yaml", latest_publications)
    index_template.add_content("presentations", options.templates, "presentations.yaml", latest_presentations)
    index_template.render_page(jinja_env, options.outdir)

    funding_template = Template("funding.html")
    funding_template.add_content("contents", options.templates, "funding.yaml")
    funding_template.render_page(jinja_env, options.outdir)

    contact_template = Template("contact.html")
    contact_template.add_content("contents", options.templates, "contact.yaml")
    contact_template.render_page(jinja_env, options.outdir)

    publications_template = Template("publications.html")
    publications_template.add_content("contents", options.templates, "publications.yaml")
    publications_template.render_page(jinja_env, options.outdir)

    presentations_template = Template("presentations.html")
    presentations_template.add_content("contents", options.templates, "presentations.yaml")
    presentations_template.render_page(jinja_env, options.outdir)

    software_template = Template("software.html")
    software_template.add_content("contents", options.templates, "software.yaml")
    software_template.render_page(jinja_env, options.outdir)


def identity(x):
    return x

class Template(object):

    def __init__(self, html_filename):
        self.html_filename = html_filename
        self.contents = {}

    # transform is a function applied to the YAML contents after it has
    # been read from file. It allows you to do some post processing of
    # the yaml contents before using it as an input to jinja
    def add_content(self, name, template_dir, yaml_filename, transform=identity):
        contents_path = os.path.join(template_dir, yaml_filename)
        with open(contents_path) as contents_file:
            yaml_contents = yaml.load(contents_file)
            self.contents[name] = transform(yaml_contents)

    def render_page(self, jinja_env, outdir):
        jinja_template = jinja_env.get_template(self.html_filename)
        rendered_html = jinja_template.render(**self.contents)
        output_filename = os.path.join(outdir, self.html_filename)
        with open(output_filename, 'w') as output_file:
            output_file.write(rendered_html)

def make_output_dir(options):
    if not os.path.exists(options.outdir):
        os.makedirs(options.outdir)


def main():
    "Orchestrate the execution of the program"
    options = parse_args()
    init_logging(options.log)
    jinja_env = init_jinja(options)
    make_output_dir(options)
    render_pages(options, jinja_env)


# If this script is run from the command line then call the main function.
if __name__ == '__main__':
    main()