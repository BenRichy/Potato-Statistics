# Automated Data Pipeline Setup

This project uses an automated pipeline to pull fresh data from Google Sheets and deploy a Shiny app to Hugging Face Spaces.

## 🏗️ Architecture Overview

```
Google Sheets ─→ GitHub Actions ─→ GitHub Repo ─→ Hugging Face Spaces
     (Live Data)     (Daily Cron)      (.RData files)    (Shiny App)
```

## 📊 Data Flow

1. **Google Sheets**: Live data source that users can update
2. **GitHub Actions**: Automated workflow runs every 4 hours
3. **GitHub Repository**: Stores processed `.RData` files  
4. **Hugging Face Spaces**: Shiny app reads data directly from GitHub

## ⚙️ Setup Instructions

### 1. Google Sheets Configuration
- ✅ Your Google Sheet is already configured
- ✅ URL: `https://docs.google.com/spreadsheets/d/1XKThbLwFV3W1njaK7lmY_SDTLs8urt78jFYkreXa7NA/edit?usp=sharing`
- ✅ Make sure it's set to "Anyone with the link can view"

### 2. GitHub Repository Setup
- ✅ GitHub Actions workflow created: `.github/workflows/update-data.yml`
- ✅ Data processing script updated: `data_analysis.R`
- ✅ App configured to read from GitHub: `app/global.R`

### 3. Repository Settings Required
Update the GitHub repository path in `app/global.R`:
```r
github_repo <- "YOUR_USERNAME/Potato-Statistics"
```

### 4. Hugging Face Spaces Deployment
When deploying to Hugging Face Spaces:
1. Upload your `app/` folder
2. Create an `app.R` file that sources the Shiny app
3. The app will automatically pull data from your GitHub repo

## 🚀 GitHub Actions Workflow

The automated workflow (`.github/workflows/update-data.yml`):

- **Trigger**: Every 4 hours + Manual trigger
- **Actions**:
  1. ✅ Installs R and dependencies
  2. ✅ Runs `data_analysis.R` script  
  3. ✅ Pulls data from Google Sheets
  4. ✅ Processes data (Bradley-Terry models, fight results, etc.)
  5. ✅ Saves `.RData` files to `app/data/`
  6. ✅ Commits and pushes changes

## 💾 Generated Data Files

The pipeline creates these files in `app/data/`:
- `raw_potato_data.RData` - Raw survey responses
- `heatmap_data.RData` - Pivot table for heatmap visualization  
- `fight_results.RData` - Head-to-head comparison results
- `bt_abilities.RData` - Bradley-Terry model rankings

## 🔧 Manual Triggers

You can manually trigger the data update:
1. Go to your GitHub repo → Actions tab
2. Click "Update Potato Data from Google Sheets"
3. Click "Run workflow"

## 🌐 Hugging Face Integration

Your Shiny app automatically:
- ✅ Attempts to download data from GitHub
- ✅ Falls back to local files if GitHub is unavailable
- ✅ Provides error handling and user feedback

## 🐛 Troubleshooting

### If the GitHub Action fails:
- Check the Actions tab for error logs
- Verify your Google Sheets is publicly accessible
- Ensure the sheet structure matches expectations (columns 3:36 for food ratings)

### If the Hugging Face app can't load data:
- Verify the `github_repo` variable in `global.R` matches your repository
- Check that `.RData` files exist in your GitHub repo
- Ensure your repository is public (or configure authentication)

## 🎯 Benefits of This Setup

✅ **Always Fresh Data**: App updates automatically when Google Sheets changes  
✅ **No Manual Deployment**: Once set up, everything runs automatically  
✅ **Free Hosting**: GitHub Actions (2000 minutes/month) + Hugging Face Spaces  
✅ **Reliable Fallbacks**: Local files used if GitHub is unavailable  
✅ **Interactive Visualizations**: Plotly integration for enhanced user experience

## 🔄 Workflow Schedule

- **Data Update**: Every 4 hours (00:00, 04:00, 08:00, 12:00, 16:00, 20:00 UTC)
- **Processing Time**: ~2-3 minutes
- **Hugging Face Sync**: Immediate (reads from GitHub)

Your potato ranking app is now fully automated! 🥔✨