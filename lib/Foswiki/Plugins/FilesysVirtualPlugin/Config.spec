#---+ Extensions
#---++ FilesysVirtualPlugin
# Foswiki topics are stored in TML (Topic Markup Language, the
# wiki markup syntax you see if you "Raw View" a topic) with embedded
# meta-data. The FilesysVirtualPlugin presents this data via a set of files,
# each of which represents a different "view" of this data. You can
# select as many views as you like, or even add your own.
# **STRING 80**
# Comma-separated list of view names. See the FilesysVirtualPlugin topic for
# a list of the available views.
$Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{Views} = 'txt';
# **PERL**
# A hash mapping of rewrite rules, used to map login names to wiki names.
# Rules are separated by commas. Rules have 
# the form:
# <pre>{
#   'pattern1' => 'substitute1', 
#   'pattern2' => 'substitute2' 
# }</pre>
# Each rule consists of a name pattern that has to match the login name to be rewritten
# and a substitute value that is used to replace the matched pattern. The
# substitute can contain $1, $2, ... , $5 to insert the first, second, ..., fifth
# bracket pair in the key pattern. (see perl manual for regular expressions).
# Example: '(.*)_users' => '$1'
$Foswiki::cfg{Plugins}{FilesysVirtualPlugin}{RewriteLoginNames} = {
  '^(.*)@.*$' => '$1'
};
