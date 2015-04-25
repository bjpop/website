# Making better scientific software

Back in 2013 Torsten Seemann published a list of [Minimum standards for bioinformatics command line tools](http://thegenomefactory.blogspot.com.au/2013/08/minimum-standards-for-bioinformatics.html) containing ten tips to make Bioinformatics command line code easier to use. The list later appeared in [GigaScience](http://www.gigasciencejournal.com/content/2/1/15]). The following article contains a few additions to Torsten's list which are inspired by my own experiences with installing, using and writing scientific software while working at [VLSCI](www.vlsci.org.au). 

While most of the ideas presented in this article apply equally well to other programming domains, I think scientific programming is one area where big improvements can be made.

## Use a standard license 

If your code is open source then use a standard license. The [Open Source Initiative](http://opensource.org/) provides a [good description](http://opensource.org/licenses) of the most common options. The best I can say for non-open source licenses is try to keep them simple: remember that humans have to read these things. 

A standard license provides minimum fuss for users and increases the chances that your software will be widely used. It is very common for research centers to install software on behalf of their users. Unsurprisingly such research centers (and their parent institutions) tend to be risk adverse when it comes to legal matters. A non-standard license is very likely to require vetting by lawyers, which can be a [protracted exercise](http://dilbert.com/strip/2013-08-11). 

Make your license easy to find. The license should be indicated prominently near the start of the user documentation. Use the common name of the license in your documentation. For example, if you have chosen the [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause) then call it by that name. The user should not have to read the license to find out what it is. Obviously you will also need to make terms of the license available for the user to read. I recommend putting it in a file called `LICENSE.txt` (or just `LICENSE`) in the top directory of the source repository. 

If your software includes external packages in its distribution then make sure the licenses for those packages are clearly indicated both in the distribution files and in the user documentation.

It may be tempting to take a standard license and add your own little twist to it, such as a "cite my work" clause. *Don't do it!* Even small adjustments will force research centers to send your license to the lawyers for scrutiny. A polite citation request in the documentation should be sufficient. Responsible users will do the right thing, especially if you make it easy for them. 

The American spelling of "license" may irritate those of us who are used to the British spelling "licence", however in the computing world American spelling tends to be the standard, so it is best to stick with that.

## Log progress and diagnostics to file

A log file can be a godsend to anyone who is trying to find out what a program has done or why it might have gone wrong. They are also incredibly useful for bug reporting. Provide a '`--log <filename>`' command-line argument which allows the user to specify the name of the log file. You may also consider allowing the log messages to optionally appear on the standard error device (`stderr`). I think a file is generally the better option because it won't get lost by some accidental output redirection, and you won't clutter `stderr` with junk. You may also want to consider allowing the user to turn logging off.

Here are some things that I think are useful to log:

 * The timestamp (date plus time of day down to seconds) of when the program started and ended.
 * The version of the program.
 * The exact command line used to run the program (this can be obtained from the argument vector). This is useful if you want to run the computation again, especially so in the distant future when you've forgotten the command line syntax.
 * The path of any file read or written which is not obvious from the command line (unless there are zillions of them).
 * Basic statistics about program execution (for example number of data points processed).
 * Warnings about unusual events and detailed error messages.
 * Milestones in the computation process.
 * Exact copies of any shell commands called by the program.

Most programming languages provide logging libraries; use one of those instead of inventing your own. In Python I recommend the standard [logging](https://docs.python.org/3/howto/logging.html) library. Good logging libraries have lots of features that are tedious to support on your own such as:

 * Automatic timestamping of messages.
 * Customisable logging levels such as info, warning, error and so forth. 
 * Proper handling of output buffering so that your log messages arrive in the file in a timely manner.
 * Support for interleaving logging messages from concurrent processes. 
 * Rolling log files.

One thing to watch out for is writing very large log files. Your program may run for a long time, or it may be used many times in parallel. Overly large log files run the risk of exhausting disk space. Consider using a rolling log file for a program which runs for a long time (e.g. a server). 

While I think it is good practice to write error messages to the log file, I think it is also important that they appear on `stderr` as well for the benefit of interactive command line users (see below for more thoughts on error messages).

## Use standard data formats

Standard data formats enable interoperability. In the very least you should try to avoid inventing your own ad-hoc formats when suitable alternatives already exist. Unless your software has special I/O performance requirements text files are preferable over binary files because they are friendlier to humans and Unix command line utilities such as `grep`. If you have special data requirements that are not well suited to text then consider [HDF5](https://hdfgroup.org/HDF5/).

[CSV (comma separated values)](http://en.wikipedia.org/wiki/Comma-separated_values) is recommended for tabular data because it is relatively simple and plays nicely with spreadsheet applications. Most programming languages provide CSV libraries; use one of those instead of inventing your own. In Python I recommend the standard [csv](https://docs.python.org/3/library/csv.html) library. It is good practice to provide column headers in CSV files because they allow you to refer to data by attribute name instead of column number (with support from your CSV library). Column headers are more robust in in the presence of column re-ordering whereas column numbers are fragile.

[XML](http://www.w3.org/XML/) was all the rage a few years ago for representing (semi) structured data, but it has fallen out of fashion in some circles because it is not so friendly for human consumption. [JSON](http://json.org/) and [YAML](http://yaml.org/) are human-friendlier alternatives which are quite good at representing nested data structures. JSON is particularly prevalent in web applications due to its Javascript origins. See the [YAML documentation](http://yaml.org/spec/1.2/spec.html#id2759572) for a comparison of all three formats. Despite its declining popularity XML still does have useful characteristics such as schema validation (plus many standard schemas) and powerful document transformation technology such as [XSLT](http://www.w3.org/TR/xslt). If your data is more like a document than a datastructure then XML may still be a better choice.

## Use meaningful version numbers

Version numbers allow users to track the provenance of their work. This is particularly important in science where reproducability is a primary concern. Torsten already mentioned the need for version numbers in item 3 his article. I want to reiterate that point because it is so important and so frequently overlooked in scientific software. I also want to add that if you are not sure how to go about versioning then consider the approach of [Semantic Versioning](http://semver.org/). This is a controversial topic in the programming world, but it is better to follow a standard system than invent your own arbitrary scheme.

## Make it easy to install and use in a shared environment

Scientific software has a justly deserved reputation for being difficult to install and use. Mostly this is down to the sloppiness of the developers and the fact that usability is a distant consideration compared to getting a result for a paper. If your code is easy to install then it is more likely to be used.

Where possible you should follow the prevailing software installation conventions. For example, if your program is written in Python then you should make it a [Python package](https://pypi.python.org/pypi) that can be installed with `pip`. If your program is in a compiled langauage, such as C, then you should use a build tool such as [make](http://en.wikipedia.org/wiki/Make_(software)), and ideally a configuration tool such as [autoconf](https://www.gnu.org/software/autoconf/). You may also consider [CMake](http://www.cmake.org/) if building on multiple platforms (such as Windows) is important for your project.

Research centers will often want to install your code from source, and they will generally want to install it in a location of their own choosing. Ideally your program's behaviour should not be context dependent:

 * Do not hard-code paths to files (item 6 in Torsten's list).
 * Do not make assumptions about your software's dependencies (item 9 in Torsten's list). This is another reason to use a packaging tool and configuration management.
 * Do not assume that your program will be installed in a specific location.
 * Do expect that your program will be run by multiple users concurrently. 
 * Do allow mutliple different versions of your program to be installed on the same system without interfering with each other.

Shared mutable state (multiple instances of your program reading and writing the same files) is especially troublesome in a concurrent setting. A classic problem occurs when programs write to files in the `/tmp` directory but do not protect against multiple instances clobbering each other's data. If you need to create temporary files then use a library designed for this purpose, such as the standard [tempfile](https://docs.python.org/3/library/tempfile.html) library from Python. Provide a command line argument that lets the user decide where temporary files should be stored. Some High Performance Computing (HPC) systems have special "scratch" filesystems for this purpose which can have non-standard paths.

If you really do need shared mutable state then consider using a database. [SQLite](http://www.sqlite.org/) does not need a database server so it is very simple to set up and use. [PostgreSQL](http://www.postgresql.org/) has all the bells and whistles you can imagine, but may be overkill for a lot of applications. Lock files should be used sparingly because they have complex semantics and are difficult to use correctly. 

When developing and testing your code consider using a sandboxing environment such as Python's [virtualenv](https://pypi.python.org/pypi/virtualenv) or perhaps even something more substantial such as [Docker](https://www.docker.com/). These can help ensure your application does not depend on idiosyncracies of your development computer.

## Use and document exit status values

It is quite common for scientific software to be used as part of a *pipeline* (or *workflow*), where the outputs of one computation are fed as inputs to another. Such pipelines can become large and complex and can run for hours or days on HPC systems, therefore the likelihood of failure is high. One of the worst things that can happen in a pipeline is for one stage to fail and produce partial or incorrect results but for subsequent stages to carry on oblivious to the error. This can result in data corruption or worse it can produce false results which go undetected. Most pipeline systems know very little about the programs they run, so they rely heavily on the exit status of each computation to decide what to do next. The Unix convention is that an exit status of zero means that the computation ran to completion successfully, and any other exit status means something exceptional happened. Therefore one of the worst sins you can commit in scientific programming is to return a misleading exit status code, especially so if you return zero when your program failed! 
You should be careful to only use a zero exit status when your program has successfully run to completion. Furthermore you should use different exit status values for different kinds of errors and be sure to *document what they mean*.

You can minimise the risk of false exit values by reducing the number of exit points in your program and by defining exit codes as constants in their own module. 

## Generate informative error messages

Few things are more infuriating than [cryptic error messages](http://en.wikipedia.org/wiki/Guru_Meditation); they tell you that something went wrong, but little else. Good error messages provide information that can help the user to find and fix the problem. 

Consider the case of a missing file. What might the user need to know?

 * What was the name of the file that was missing?
 * In which places did the program search when it tried to find the file?
 * How can the user influence the places that are searched? 

Your program might be called from a pipeline or a shell script where its output, including errors, are mixed together from other programs. Therefore your error messages should include the name of your program to clearly identify the source of the error.

Here is a template that caters for many kinds of errors in an informative way:

```
    program name ERROR: general description of error

        What happened.

        How the user could fix the problem.
```

for example, assuming the program is called `frobnicate`:

```
    frobnicate ERROR: could not find configuration file.

        Tried to read configuration file /home/foo/frobnicate.config
        File does not exist.

        Create a configuration file called /home/foo/frobnicate.config
        or specify an alternative path with --config <filename>
```


## Follow command line argument conventions

## Input and output filenames should be parameters


## Avoid writing lots of little files


