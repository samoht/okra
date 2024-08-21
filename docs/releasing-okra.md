# Releasing okra

## Why is this important?

[okra](https://github.com/tarides/okra) is actively used by Tarides engineers to generate and lint weekly reports. Sometimes, this includes changes to the format of the weekly submissions and updating the tool to lint these properly. Hence, rolling out new releases must be communicated very clearly. 

Without clear communication, it creates confusion among engineers about which version to use, resulting in a split between engineers using different versions of okra. This also creates problems downstream for the ops team that processes the reports, who will have to deal with discrepancies between the reporting formats, leading to tedious and time consuming back and forth between the engineers.

Eventually, we want to move towards a place where the manual processing of reports is eliminated, and we can depend on the clear process and linting to automatically process the reports. For this, clear communication around releases is an essential prior step.

## Communicating to okra users about releases

Engineers should be able to find the version of the okra tool to be used in the tarides/admin repo README.md. For example, there is a table which says

| Okra version | Year and week |
| -------------|---------------|
| 1.0.0 | 2024 W22 (May 29)    |
| 2.0.0 | 2024 W23 (June 4)    |
| 2.0.1 | 2024 W23 (June 5)    | 
| 2.1.0 | 2024 W25 (June 19)   |
| 3.0.0 | 2024 W27 (July 4)    |
| 3.1.0 | 2024 W29 (July 17)   |

The engineers are requested to submit their weeklies once a month before the 5th. The ops team processes the weeklies once a month, between the 5th and 10th. Then, the data is handed over to the finance team. Hence, it is important that breaking changes are not introduced in the middle of the month. This makes it harder for both engineers (who will have to lint using two different versions of okra for a month) and ops folks who will have to process reports differently. 

## Cutting a release

* Ensure that breaking releases are rolled out on a future date, ideally in the first week of a particular month.
* Follow the release procedure in [CONTRIBUTING.md](../CONTRIBUTING.md).
* Add the information to the tarides/admin README.md table.
* Make an announcement post on #tarides-internal-tooling with a clear title about the new release, when the release should be used by okra users, and have time for questions and discussions. Post the same on #tarides-announces. Ideally, the releases are at a future date.
