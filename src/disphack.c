calculate_id_lines ()
{
  register int i, j;
  int *match, *insdel;
  int prevmatch = 0;
  int ded_reckon = 0;

  match = (int *) alloca (ScreenLength * sizeof (int));
  insdel = (int *) alloca (ScreenLength * sizeof (int));

/* Here should check for last few lines matching exactly,
or all except last two.  */

  for (i = 1; i <= ScreenLength; i++)
    {
      match[i] = ++ded_reckon;
      if (PhysScreen[i]->draw_cost < 10)
	continue;
      for (j = matchmin; j <= ScreenLength; j++)
	{
	  if (PhysScreen[i] == DesiredScreen[j]
	      || (PhysScreen[i]->hash == DsiredScreen[j]->hash))
	    {
	      match[i] = j;
	      ded_reckon = j;
	      matchmin = j + 1;
	      break;
	    }
	}
    }

  
  /* Now match[i] is the line (origin-1)
     that line i should move to.  */

  deficit = 0;
  ded_reckon = 1;
  for (i = 1; i <= window_size; i++)
    {
      insdel = match[i] - ded_reckon;
      ded_reckon = match[i] + 1;
      if (insdel < 0)
	{
	  topos (i - deficit + insdel, 1);
	  deletelines (- insdel);
	  deficit -= insdel;
	}
    }

  ded_reckon = 1;
  for (i = 1; i <= window_size; i++)
    {
      insdel = match[i] - ded_reckon;
      ded_reckon = match[i] + 1;
      if (insdel > 0)
	{
	  topos (i - deficit, 1);
	  deletelines (insdel);
	  deficit -= insdel;
	}
    }

  if (deficit != 0)
    abort ();

  for (i = 1; i < window_size; i++)
    PhysScreen[i] = 0;
  for (i = 1; i < window_size; i++)
    PhysScreen[match[i]] = OPhysScreen[i];
}
