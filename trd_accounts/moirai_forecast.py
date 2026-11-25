# La Societe Nouvelle
#
# Python bridge for Moirai-1.1-R-small forecasting (uni2ts), called from R via
# reticulate through utils_moirai_forecasts.R.

import numpy as np
import pandas as pd
from gluonts.dataset.pandas import PandasDataset
from uni2ts.model.moirai import MoiraiForecast, MoiraiModule


def moirai_load_module(model_id):
    # Downloads and caches weights on first call. Reuse across series.
    return MoiraiModule.from_pretrained(model_id)


def moirai_predict(
    module,
    values,
    start_year,
    prediction_length,
    context_length,
    patch_size="auto",
    num_samples=100,
):
    # Returns prediction_length values (mean of sampled trajectories).
    values = np.asarray(values, dtype="float64")

    dates = pd.period_range(start=str(int(start_year)), periods=len(values), freq="Y")
    series = pd.Series(values, index=dates.to_timestamp())
    df = pd.DataFrame({"target": series})

    dataset = PandasDataset(df, target="target", freq="Y")

    model = MoiraiForecast(
        module=module,
        prediction_length=int(prediction_length),
        context_length=int(min(context_length, len(values))),
        patch_size=patch_size,
        num_samples=int(num_samples),
        target_dim=1,
        feat_dynamic_real_dim=0,
        past_feat_dynamic_real_dim=0,
    )

    predictor = model.create_predictor(batch_size=1)
    forecast_it = predictor.predict(dataset)
    forecast = next(iter(forecast_it))

    return forecast.mean.tolist()
