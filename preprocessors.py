
gender_levels = {"male":1, "female":2}
agerange_levels = {"18-20":1, "21-24":2, "25-34":3, "35-44":4, "45-54":5, "55-64":6, "65+":7}

def normalizer(record, feature, feature_levels, default=0):
    return feature_levels.get(record[feature],default)

def normalize_age(record, feature):
    try:
        return float(record[feature])
    except:
        return 0
    return 

def socially_active(record, newrecord):
    feature = 'socially_active'
    #
    try:
        value = int(record['hasfacebook'] or '0') + int(record['haslinkedin'] or '0')
    except:
        import pdb; pdb.set_trace()
    return (feature, value)

def business_age_lie(record, newrecord):
    feature = 'business_age_lie'
    value = 0
    #
    age = newrecord["age"]
    businessage = newrecord["businessage"]
    if age != 0 and businessage != 0:
        diff = age-businessage
        if businessage > age:
            value = 10
        elif diff < 10:
            value = 8
        elif diff < 15:
            value = 5
        elif diff < 18:
            value = 2
    return (feature, value)

