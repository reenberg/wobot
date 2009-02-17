#include "ISensorEasy.h"
#include <iostream>

int main(){
  // Get the ERSP resource manager
  Evolution::Result result;
  Evolution::ResourceManager *rm;
  rm = new Evolution::ResourceManager( NULL, &result ); 
  if( result != Evolution::RESULT_SUCCESS ) {
    throw "Cannot create a resource manager";
  }

  ISensorEasy *s = new ISensorEasy(rm);
  printf("sensor success!\n");
  bool firsttime = true;
  while (true) {
    Evolution::Timestamp *timestamp;
    double r[12];
	if (firsttime) {
		std::cout << "bn_ene\t" << "bn_wnw\t" << "bn_n\t"
				 << "bn_ne\t" << "bn_nw\t" << "te_nnw\t" << "te_nne\t"
				 << "tw_nnw\t" << "tw_nne\t" << "bs_w\t" << "bs_e\t" 
				 << "bw_s\t" << "be_s\t\n";
		firsttime = false;
	}
	
	s->bn_ene->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[0]);
	s->bn_wnw->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[1]);
	s->bn_n->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[2]);
	s->bn_ne->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[3]);
	s->bn_nw->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[4]);
	s->te_nnw->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[5]);
	s->te_nne->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[6]);
	s->tw_nnw->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[7]);
	s->tw_nne->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[8]);
	s->bs_w->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[9]);
	s->bs_e->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[10]);
	s->bw_s->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[11]);
	s->be_s->get_distance_reading(Evolution::NO_TICKET, timestamp, &r[12]);

	double tmp;
	for(int i = 0; i < 13; i++) {
		tmp = r[i]/100;
		if (tmp > 0.8) tmp = 0.8;
		printf("%f\t", tmp);
	}
	std::cout << std::endl;
	sleep(1);
    }
  return 0;
}
