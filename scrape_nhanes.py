import requests
import re
import os
import pathlib
import xport

html = requests.get('https://wwwn.cdc.gov/nchs/nhanes/Default.aspx').text
DATA_PATH = os.path.expanduser('~/emoryhealthcare-project/data')

def get_survey_data(year):
    components = ['Demographics', 'Dietary', 'Examination', 'Laboratory', 'Questionnaire', 'LimitedAccess']
    for component in components:
        survey_urls = re.findall(rf'href=\"(.*Component={component}.*CycleBeginYear={year})', html)
        for survey_url in survey_urls:
            survey_url = survey_url.replace('&amp;', '&')
            survey_url = f'https://wwwn.cdc.gov{survey_url}'
            survey_page = requests.get(survey_url).text
            data_urls = re.findall(rf'href=\"(.*XPT|.*xpt)\"', survey_page)
            for data_url in data_urls:
                data_url = f'https://wwwn.cdc.gov{data_url}'
                r = requests.get(data_url)
                xpt_filename = os.path.basename(data_url)
                filepath = os.path.join(DATA_PATH, f'{year}-{year+1}', component)
                pathlib.Path(filepath).mkdir(parents=True, exist_ok=True)
                xpt_filepath = os.path.join(filepath, xpt_filename)
                with open(xpt_filepath, 'wb') as f:
                    f.write(r.content)
                with open(xpt_filepath, 'rb') as f:
                    data = xport.to_dataframe(f)
                if xpt_filename.endswith('.XPT'):
                    csv_filename = xpt_filename.replace('.XPT', '.csv')
                elif xpt_filename.endswith('.xpt'):
                    csv_filename = xpt_filename.replace('.xpt', '.csv')
                csv_filepath = os.path.join(filepath, csv_filename)
                print(f'Downloading to {csv_filepath}')
                data.to_csv(csv_filepath, index=False)
                os.remove(xpt_filepath)


if __name__ == '__main__':
    years = [y for y in range(1999, 2016, 2)]
    # #years = [2015]
    for year in years:
        get_survey_data(year)
