#include <libplayerc++/playerc++.h>
#include <iostream>
#include <unistd.h>

std::string  gHostname(PlayerCc::PLAYER_HOSTNAME);
uint         gPort(PlayerCc::PLAYER_PORTNUM);
uint         gIndex(0);
uint         gDebug(0);

void print_usage(int argc, char** argv);

int parse_args(int argc, char** argv)
{
  // set the flags
  const char* optflags = "h:p:i:d:";
  int ch;

  // use getopt to parse the flags
  while(-1 != (ch = getopt(argc, argv, optflags)))
  {
    switch(ch)
    {
      // case values must match long_options
      case 'h': // hostname
          gHostname = optarg;
          break;
      case 'p': // port
          gPort = atoi(optarg);
          break;
      case 'i': // index
          gIndex = atoi(optarg);
          break;
      case 'd': // debug
          gDebug = atoi(optarg);
          break;
//      case '?': // help
//      case ':':
      case '?': 
          print_usage(argc, argv);
          exit (-1);
      default:  // unknown
	  if (isprint (optopt))
	     fprintf (stderr, "Unknown option `-%c'.\n", optopt);
	  else
             fprintf (stderr, "Unknown option character `\\x%x'.\n", optopt);
          print_usage(argc, argv);
          exit (-1);
    }
  }
  return (0);
} // end parse_args



void print_usage(int argc, char** argv)
{
  using namespace std;
  cerr << "USAGE:  " << *argv << " [options]" << endl << endl;
  cerr << "Where [options] can be:" << endl;
  cerr << "  -h <hostname>  : hostname to connect to (default: "
       << PlayerCc::PLAYER_HOSTNAME << ")" << endl;
  cerr << "  -p <port>      : port where Player will listen (default: "
       << PlayerCc::PLAYER_PORTNUM << ")" << endl;
  cerr << "  -i <index>     : device index (default: 0)"
       << endl;
  cerr << "  -d <level>     : debug message level (0 = none -- 9 = all)"
       << endl;
} // end print_usage
