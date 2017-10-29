'''
Module      : Main 
Description : The main entry point for the program.
Copyright   : (c) Bernie Pope, 2017 
License     : MIT 
Maintainer  : bjpope@unimelb.edu.au
Portability : POSIX

Convert the CSV exported from Paperpile to JSON format
'''

from argparse import ArgumentParser
import sys
import logging
import pkg_resources
import yaml
import csv


EXIT_FILE_IO_ERROR = 1
EXIT_COMMAND_LINE_ERROR = 2
DEFAULT_VERBOSE = False
DEFAULT_TEMPLATES_DIR = 'templates' 
DEFAULT_OUTPUT_DIR = 'docs' 
PROGRAM_NAME = "paperpile_csv_to_yaml"


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
    parser.add_argument('input_csv',
        metavar='INPUT_CSV',
        type=str,
        help='input CSV file containing Paperpile export data',
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

def split_authors(authors):
    if type(authors) is str:
        return authors.split(',')
    else:
        return None

def split_urls(urls):
    if type(urls) is str:
        urls = urls.split(';')
        if len(urls) > 0:
            chosen_url = urls[0]
            for url in urls:
                if 'dx.doi.org' in url:
                    chosen_url = url
                    break
            return chosen_url

    return '' 

def csv_to_yaml(csv_filename):
    with open(csv_filename) as file:
        reader = csv.DictReader(file)
        result = []
        for row in reader:
            row = dict(row)
            entry = { 'authors': split_authors(row['Authors']),
                'journal': row['Full journal'],
                'abstract': row['Abstract'],
                'doi': row['DOI'],
                'type': row['Item type'],
                'keywords': row['Keywords'],
                'pmid': row['PMID'],
                'year': row['Publication year'],
                'title': row['Title'],
                'volume': row['Volume'],
                'url': split_urls(row['URLs']),
                'pages': row['Pages'],
                'issue': row['Issue'],
                'date': row['Date published'] }
            result.append(entry)
        result.sort(key=lambda x: int(x['year']), reverse=True)
        print(yaml.dump(result, default_flow_style=False))

def main():
    "Orchestrate the execution of the program"
    options = parse_args()
    init_logging(options.log)
    csv_to_yaml(options.input_csv)


# If this script is run from the command line then call the main function.
if __name__ == '__main__':
    main()
