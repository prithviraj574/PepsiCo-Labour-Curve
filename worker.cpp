#include <bits/stdc++.h>
#define pii pair<double, pair<int, double> > // startTime,  numOfWorker, currentTimeReaminingOfWorker

#define mp make_pair

using namespace std;

int main() {
	
	int A[6];
	
	float duration[7] = {0, 4.5, 9, 12.5, 16.5, 20.5, 24};
	int time;
	cin>>time;

	for (int i = 0; i < 6; ++i){
		cin>>A[i];
	}

	priority_queue<pii, vector<pii>, greater<pii> > q;

	int numOfWorker = 0;

	int currentWorker = 0;
	int currentTimeReaminingOfWorker = 480;

	for (int i = 0; i < 6; ++i){
		double timeRequired = A[i] * time;
		double timeToLoadTruck = (duration[i+1] - duration[i]) * 60;

		int x  = ceil(timeRequired / timeToLoadTruck);
		int timeRequiredPerWorker = timeRequired / x ;

		if(q.empty()){
			double callAt = duration[i] + (timeToLoadTruck - timeRequiredPerWorker) / 60;
			cout << "call " << x << " labour at " << floor(callAt) << ":" << (callAt - floor(callAt))*60 << endl;

			currentTimeReaminingOfWorker = 480 - timeRequiredPerWorker;
			currentWorker = x;
			cout << timeRequired << " " << timeToLoadTruck << " " <<  timeRequiredPerWorker << endl;
			q.push(mp(callAt, mp(x, currentTimeReaminingOfWorker)));

			numOfWorker += x;
		}
		else{
			priority_queue<pii, vector<pii>, greater<pii> > q1;

			while(!q.empty() && timeRequired > 0){
				pii p = q.top(); q.pop();
				currentTimeReaminingOfWorker = p.second.second;
				currentWorker = p.second.first;

				if(currentTimeReaminingOfWorker >= timeToLoadTruck) {
					timeRequired -= currentWorker * timeToLoadTruck;
					if(currentTimeReaminingOfWorker - timeToLoadTruck > 0)
						q1.push(mp(p.first, mp(currentWorker, currentTimeReaminingOfWorker - timeToLoadTruck)));
				}
				else{
					timeRequired -= currentWorker * currentTimeReaminingOfWorker;
				}
			}
			while(!q.empty()){
				pii p = q.top(); q.pop();
				currentTimeReaminingOfWorker = p.second.second;
				currentWorker = p.second.first;
				if(currentTimeReaminingOfWorker - timeToLoadTruck > 0)
					q1.push(mp(p.first, mp(currentWorker, currentTimeReaminingOfWorker - timeToLoadTruck)));
			}

			q = q1;

			// cout << i << " " <<  timeRequired << " " << timeToLoadTruck << endl;

			if(timeRequired > 0) {
				x  = ceil(timeRequired / timeToLoadTruck);
				timeRequiredPerWorker = timeRequired / x ;

				double callAt = duration[i] + (timeToLoadTruck - timeRequiredPerWorker) / 60;
				cout << "call " << x << " labour at " << floor(callAt) << ":" << (callAt - floor(callAt))*60 << endl;

				currentTimeReaminingOfWorker = 480 - timeRequiredPerWorker;
				currentWorker = x;
				cout << timeRequired << " " << timeToLoadTruck << " " <<  timeRequiredPerWorker << endl;
				q.push(mp(callAt, mp(x, currentTimeReaminingOfWorker)));

				numOfWorker += x;
			}
		}
	}

	cout << numOfWorker << endl;

	return 0;
}